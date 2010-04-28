package org.riedelcastro.thebeast.util

import collection.mutable.{LinkedHashMap, ArrayBuffer}

/**
 * @author Sebastian Riedel
 */


trait Tracker {
  def marked(text: String, start: Boolean)
}

class PrintingTracker extends Tracker {
  def marked(text: String, start: Boolean) = {
    print(System.currentTimeMillis + " ")
    println(if (start) "Start: " + text else "End: " + text)
  }
}

class TimingTracker extends Tracker {
  private val starts = new ArrayBuffer[Long]
  private val texts = new ArrayBuffer[String]

  def marked(text: String, start: Boolean) = {
    if (start) {
      starts += System.currentTimeMillis
      texts += text
    } else {
      val duration = System.currentTimeMillis - starts.remove(starts.size - 1)
      val text = texts.remove(texts.size - 1)
      println("Track " + text + " took " + duration + "ms")
    }
  }
}

class TimingCollector extends Tracker {
  private val starts = new ArrayBuffer[Long]
  private val durations = new LinkedHashMap[String, Long]
  private val counts = new LinkedHashMap[String, Int]
  private val texts = new ArrayBuffer[String]

  def marked(text: String, start: Boolean) = {
    if (start) {
      starts += System.currentTimeMillis
      texts += text
    } else {
      val duration = System.currentTimeMillis - starts.remove(starts.size - 1)
      val text = texts.remove(texts.size - 1)
      durations(text) = durations.getOrElse(text, 0l) + duration
      counts(text) = counts.getOrElse(text, 0) + 1
    }
  }

  def timings = durations.keySet.map(text =>
    text +
        "\n\tTotal Time: " + durations(text) +
        "\n\tTotal Calls: " + counts(text) +
        "\n\tAvg. Time: " + durations(text).toDouble / counts(text)).mkString("\n")
}

object TimingCollector extends TimingCollector

object Timer {
  private var last = 0l

  def delta = {val now = System.currentTimeMillis; val result = now - last; last = now; result}

  def start = delta

}


object Trackers extends ArrayBuffer[Tracker] {
  //this += new TimingTracker 


}


trait Trackable {
  def mark(text: String, start: Boolean) = {for (t <- Trackers) t.marked(text, start)}

  def |**(text: String) = mark(text, true);
  def **|(text: String) = mark(text, false);
  def **| = mark("NO MESSAGE", false);

}