package com.gatis.bootcamp.project.zole

// score = spēles punkti
case class Player private (name: String, id: String, score: Int) {
  def updateScore(points: Int) = copy(score = score + points)

  // override def toString = s"$name"
  // for ease while developing
  override def toString = s"$id-$name"
}

object Player {
  def of(name: String, id: String) = Player(name, id, 0)
}
