package com.gatis.bootcamp.project.zole

case class Player private (name: String, id: String, score: Int) {
  def updateScore(points: Int) = copy(score = score + points)

  // override def toString = s"$name"
  override def toString = s"$id-$name" // for ease while developing
}

object Player {
  def of(name: String, id: String) = Player(name, id, 0)
}
