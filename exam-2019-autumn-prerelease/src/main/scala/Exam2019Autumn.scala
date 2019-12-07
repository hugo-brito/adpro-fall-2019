/************************************************************************
  Final Exam: Advanced Programming (Master-Level, MSc-Level)
  IT University of Copenhagen, Autumn 2019: 16 December 9:00
  by Andrzej Wąsowski

  Your Full Name: ___
  Your ITU email account: ___

  The exam consists of 3 parts  encompassing 15 questions to be solved
  within 4 hours.  MSc students solve all parts (K-CS, K-SDT, guests).
  Master of Software Engineering students (M-SEN) solve only the first
  2 parts.

  The three parts are completely  independent.  Within the parts it is
  possible  to answer  later questions,  missing the  answers for  the
  previous ones, but it is recommended to answer questions within part
  in order.  This should be most efficient.

  You can  use any function  from the course (textbook,  exercises) in
  the  solutions, as  well  as standard  library  functions.  You  can
  access any written or electronic  material, also online, but you are
  not allowed to communicate with anybody during the exam.

  By  submitting,  you declare  to  have  solved the  problems  alone,
  without communicating with anybody.

  SUBMISSION

  Solve the tasks in the file 'Exam2019Autumn.scala' (this file) found
  in the zip archive made available on LearnIt.

  Fill in your name and your ITU email above, in the top of the file.

  Submit this file  and only this file to learnIT.   Do not convert it
  to  any other  format than  .scala.  Do  not submit  the entire  zip
  archive. Do  not reorder  the answers,  and do  not remove  question
  numbers from the  file.

  The only accepted file format is '.scala'.

  Keep the solutions within 80 columns width to facilitate grading.

  ADVICE

  The  answers  will   be  graded  manually. We  will   focus  on  the
  correctness of ideas and the use  of the course concepts. We will be
  permissive on  minor issues  such as semicolons,  other punctuation,
  small deviations  in function  names, switching between  curried and
  not  curried arguments,  etc.  We  will not  check whether  the type
  inference succeeds.   It suffices  that a  human reader  could infer
  types.

  We do not recommend solving questions to the point when they compile
  and pass tests.  Dependency problems  and other technical issues can
  take a lot of time, so only do this, once you are done with drafting
  all answers.

  Nevertheless, if  you do compile,  you can use the  'build.sbt' file
  provided  in the  zip  archive linked  above. It  has the  necessary
  library dependencies  configured. The zip archive also  contains the
  course libraries that the solutions depend on.

  Good luck!

*************************************************************************/

package adpro

import scala.language.higherKinds
import scala.language.implicitConversions

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._

import org.scalacheck.Arbitrary
import org.scalatest.prop._

import com.cra.figaro.language.{Element, Constant, Flip, Universe, Select}
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous.{Beta, AtomicBeta}
import com.cra.figaro.library.atomic.discrete.{Binomial,Uniform}
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.sampling.{Importance}
import com.cra.figaro.algorithm.factored.{VariableElimination}

import fpinscala.monoids.Monoid
import fpinscala.monads._
import fpinscala.laziness.{Stream,Empty,Cons}
import fpinscala.laziness.Stream._
import fpinscala.parallelism._
import fpinscala.parallelism.Par._
import adpro.data._
import adpro.data.FingerTree._
import monocle.{Lens,Optional}

object Exam2019Autumn {

  object Game {

    lazy val s: Stream[FingerTree[Int]] = Stream ()

  }

}
