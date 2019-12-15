// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._

// This
// (https://stackoverflow.com/questions/40015416/spark-unable-to-load-native-hadoop-library-for-your-platform)
// actually does seems to work, to eliminate the missing hadoop message.
// 'WARN NativeCodeLoader: Unable to load native-hadoop library for your platform... using builtin-java classes where applicable'
// AW not sure if the 'hadoop missing warning' matters though.

object Main {

	type Embedding = (String, Vector)
	type ParsedReview = (Integer, String, Double)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[5]")
		.getOrCreate

	spark.conf.set("spark.executor.memory", "16g")

	import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text and summary into a single text column

	def loadReviews (path: String): Dataset[ParsedReview] =
		spark
			.read
			.schema (reviewSchema)
			.json (path)
			.rdd
			.zipWithUniqueId
			.map[(Integer,String,Double)] { case (row,id) =>
					(id.toInt, s"${row getString 2} ${row getString 0}", row getDouble 1) }
			.toDS
			.withColumnRenamed ("_1", "id" )
			.withColumnRenamed ("_2", "text")
			.withColumnRenamed ("_3", "rating")
			.as[ParsedReview]

	// Load the GLoVe embeddings file

	def loadGlove (path: String): Dataset[Embedding] =
		spark
			.read
			.text (path)
			.map  { _ getString 0 split " " }
			.map  (r => (r.head, Vectors.dense(r.tail.map (_.toDouble)))) // Nice!
			.withColumnRenamed ("_1", "word" )
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]

	// Takes a datasets and splits the column with text into a Sequence of words instead.

	def cleanReviews (ds :Dataset[ParsedReview]) :Dataset[Row] = {
		// To get our tokenizer completely right
		// https://nlp.stanford.edu/software/tokenizer.html
		new RegexTokenizer()
			.setInputCol("text")
			.setOutputCol("words")
			.setPattern("[^a-zA-Z0-9_]+") //Ensures that we still keep words like "it's", but get rid of ".", "," etc.
			.transform(ds)
			.select("id", "words", "rating")
	}

	// Splits the text into seperate words and adds a count for each word.

	def splitText (ds :Dataset[Row]) :Dataset[Row] = ds
		.flatMap(r => r.getSeq[String](1)
		.map(i => (r.getInt(0), i, r.getDouble(2), 1.0))) //maps on the seq
		.withColumnRenamed ("_1", "id" )
		.withColumnRenamed ("_2", "words")
		.withColumnRenamed ("_3", "rating")
		.withColumnRenamed ("_4", "count")

	// Sums all vectors that are related to a given id.
	// This is very similar to the solution in this link: 
	// https://stackoverflow.com/questions/33899977/how-to-define-a-custom-aggregation-function-to-sum-a-column-of-vectors

	def aggregateVectors (ds :Dataset[Row], vecSize :Int) :Dataset[Row] = ds
		.select("id", "vec")
		.rdd
		.map{ case Row(id: Int, v: Vector) => (id, BDV(v.toDense.values)) }
		.foldByKey(BDV.zeros[Double](vecSize))(_ += _)
		.mapValues(v => Vectors.dense(v.toArray))
		.toDF("idvec", "vec")

	// Takes a rating of 5 and converts into a representation of 3.

	def ratingMap (rating : Double) : Int = rating match {
		case 3.0 => 1
		case r if (r < 3.0) => 0
		case _ => 2
	}

	// Takes a vector and divides every element with the given number.

	def makeAverageVector (vec :Vector) (num :Double) :Vector = {
		val arr = vec.toArray.map(_ / num)
		Vectors.dense(arr)
	}

	// Takes two datasets and makes a new dataset that contains the correct
	// format for training

	def correctFormat (aggVec :Dataset[Row], wordCount :Dataset[Row]) :Dataset[Row] = {
		wordCount
			.join(aggVec, $"id" === $"idvec")
			.map {r =>
				val id = r.getAs[Int]("id"); val rat = r.getAs[Double]("rating")
				val newrat = ratingMap(rat)
				val vector = r.getAs[Vector]("vec"); val num = r.getAs[Double]("sum(count)")
				val newVector = makeAverageVector(vector) (num)
				(id, newrat, newVector)
			}
			.toDF("id", "label", "features")
	}

	def main(args: Array[String]) = {

		val DATA_PATH = "data/"

		// the data to be used for the k-fold and the perceptron
		val REVIEW_DATASET = "test.json"

		// the output will be saved as a csv file with the name of the input file used
		val OUTPUT = s"${DATA_PATH}/output/${REVIEW_DATASET}"

		val glove  = loadGlove (s"${DATA_PATH}glove.6B/glove.6B.50d.txt")
		val reviews = loadReviews (s"${DATA_PATH}reviews/Musical_Instruments_5.json")

		val cleanedReviews = cleanReviews(reviews)

		// Joins together the gloves and cleaned reviews and outputs each word with the associated
		// vector from the glove.
		val reviewWithVectors = splitText(cleanedReviews).join(glove, $"words" === $"word")
			.select("id", "rating", "word", "vec", "count")

		val aggregatedVectors = aggregateVectors(reviewWithVectors, 50)

		// Simply groups one the id and rating and uses an aggregate function to combine all the count columns.
		val wordCount = reviewWithVectors
			.groupBy("id", "rating")
			.agg(sum("count"))

		val dataSet = correctFormat (wordCount, aggregatedVectors)

		val splits = dataSet.randomSplit(Array(0.9, 0.1), seed = 1234L)
		val train = splits(0)
		val test = splits(1)

		val trainer = new MultilayerPerceptronClassifier()
			.setMaxIter(50)
			.setLayers(Array[Int](50, 5, 4, 3))
			.setBlockSize(128)
			.setSeed (1234L)

		val paramGrid = new ParamGridBuilder().build()
		val evaluator = new MulticlassClassificationEvaluator().setMetricName("accuracy")
		val cv = new CrossValidator().setEstimator(trainer) // our MultiLayerPerceptronClassifier
		.setEvaluator(evaluator)
		.setEstimatorParamMaps(paramGrid).setParallelism(11).setNumFolds(10); // tried setParallelism(11) to see if there was any improvement in the time
		val model = cv.fit(train);
		val result = model.transform(test)  
		result.map( r => (r.getInt(0), r.getInt(1), r.getAs[Vector](2).toString, r.getAs[Vector](3).toString, r.getAs[Vector](4).toString, r.getDouble(5))).withColumnRenamed ("_1", "id" )
		.withColumnRenamed ("_2", "label")
		.withColumnRenamed ("_3", "features")
		.withColumnRenamed ("_4", "rawPrediction")
		.withColumnRenamed ("_5", "probability")
		.withColumnRenamed ("_6", "prediction")
		.coalesce(1)
		.write
		.option("header","true")
		.option("sep",",")
		.mode("overwrite")
		.csv("output/musical_instr.csv")
		

		spark.stop
	}
}

/*
	1. What data sets have you managed to run (including size)?

		(5-core files)
		Musical Instruments : size (10,261 reviews)
		Toys and Games : size (167,597 reviews)

	2. What accuracy have you obtained? With how many iterations? And with what layer configuration?
	Show the variance of accuracy observed in cross validation.

		Musical Instruments : 
			accuracy: 86.64%
			Layer config: 50, 5, 4, 3
			Iteration: 1
		Toys and Games : 
			accuracy: 83.96%
			Layer config: 50, 5, 4, 3
			Iteration: 1

		Due to the limitation of time we were only able to run through each data set once and not play
		around with the layers to optimize our prediction, hence evaluating the accuracy did not make
		sense on only 2 obsevations.

	3. What degree of parallelization was obtained? (for instance contrast the wall clock time with
	the CPU time). Note we are not asking you to optimize parallelization, just to report what you
	obtained.

		We played around with the .setParallelism(11), but were unable to report on wall time vs. CPU 
		time. Hence we suspect we have achieved some level of parallelization, but unable to document
		it, due to the time.

	4. What extensions (if any) you have implemented? Have you tried another classifier? Another
	network configuration? Running on a highly parallel cluster of machines, etc. ...

		We have not tried any extension, as we were having issues simply getting it to run.

*/