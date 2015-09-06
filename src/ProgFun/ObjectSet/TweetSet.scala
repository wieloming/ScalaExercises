package ProgFun.ObjectSet

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = this.filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit

  def empty: Boolean
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def union(that: TweetSet): TweetSet = that

  override def descendingByRetweet: TweetList = Nil

  override def mostRetweeted: Tweet = throw new NoSuchElementException

  override def empty = true
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(elem)) left.filterAcc(p, right.filterAcc(p, acc.incl(elem)))
    else left.filterAcc(p, right.filterAcc(p, acc))
  }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def union(that: TweetSet): TweetSet = {
    left.union(right.union(that)).incl(elem)
  }

  override def descendingByRetweet: TweetList = {
    new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)
  }

  override def mostRetweeted: Tweet = {
    val leftMost = left.mostRetweeted
    val rightMost = right.mostRetweeted

    val leftSum = leftMost.retweets
    val rightSum = rightMost.retweets
    val elemSum = elem.retweets

    if(leftSum > rightSum){
      if(leftSum > elemSum) leftMost
      else elem
    }else if(rightSum > leftSum){
      if(rightSum > elemSum) rightMost
      else elem
    }else {
      if(rightSum > elemSum) rightMost
      else elem
    }
  }

  override def empty = false
}

trait TweetList {
  def head: Tweet

  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader
    .allTweets
    .filter(tweet => google.exists(dictWord => tweet.text.contains(dictWord)))
  lazy val appleTweets: TweetSet = TweetReader
    .allTweets
    .filter(tweet => apple.exists(dictWord => tweet.text.contains(dictWord)))

  lazy val trending: TweetList = {
    googleTweets.union(appleTweets).descendingByRetweet
  }
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
