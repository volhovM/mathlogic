package com.volhovm.mathlogic.util

/**
 * @author volhovm
 *         Created on 9/25/14
 */

object TimeStampUtil {
  var curTime = System.currentTimeMillis()
  var timeList = List[String]()
  def memTime(a: String = "") = {
    timeList = (a + ": " + (System.currentTimeMillis() - curTime)) :: timeList
    curTime = System.currentTimeMillis()
  }
  def dumpTime(): Unit = {
    for (i <- timeList.reverse) println(i)
  }
}
