package tm.coin

import java.security.spec.ECGenParameterSpec
import java.security.{KeyPair, KeyPairGenerator}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Miner extends App {

  private val keyPairGenerator: KeyPairGenerator = java.security.KeyPairGenerator.getInstance("EC")
  val ecsp = new ECGenParameterSpec("secp256k1")
  keyPairGenerator.initialize(ecsp)
  private val pair: KeyPair = keyPairGenerator.generateKeyPair()

  println("0x"+pair.getPublic.getEncoded.foldLeft(""){_ + "%02x".format(_)})
  println("0x"+pair.getPrivate.getEncoded.foldLeft(""){_ + "%02x".format(_)})
  // create a tx
  // take any other txs we know about
  // address
  // start trying to hash the data into a block
  // if we find one with a prefix, commit it (broadcast it) and start on another block
  val reward = Array[Byte](0x00,0x01,0x02,0x03,0x04)
//  val address = Address()
  val digestAlgo = "SHA-256"
  val blocks: mutable.ListBuffer[Block] = mutable.ListBuffer[Block]()
  val genesis = Array.fill[Byte](32)(0x00)
  val difficulty: Array[Byte] = Array(0xFF.toByte, 0xFF.toByte, 0xFF.toByte) // first 8 bits should be 0

  def isCorrectDifficulty(hash: Array[Byte], difficulty: Array[Byte]): Boolean = {
    difficulty.zip(hash).foldLeft(true) {
      case (s, r) => s && ((r._1 & r._2) == 0x00)
    }
  }

  def mine(data: Array[Byte], tonce: Int): Array[Byte] = {
    java.security.MessageDigest.getInstance(digestAlgo).digest(BigInt(tonce).toByteArray ++ data) match {
      case valid if isCorrectDifficulty(valid, difficulty) => valid
      case _ => mine(data,tonce+1)
    }
  }

  def run(blocks: ListBuffer[Block], prevHash: Array[Byte], reward: Array[Byte], height: Int): ListBuffer[Block] = {
    val blockHash = mine(prevHash ++ reward, 1)
    println(s"Height: $height, block ${hashToString(blockHash)}, previous ${hashToString(prevHash)}")
    height match {
      case _ if height >= 20 => blocks
      case _ => run(blocks :+ Block(blockHash, prevHash, List(reward)), blockHash, reward, height+1)
    }
  }

  def hashToString(a: Array[Byte]) = {
    "0x" + a.foldLeft(""){_ + "%02x".format(_)}
//    a.map(b => String.format("%8s", Integer.toBinaryString(b & 0xFF)).replace(' ', '0')).mkString("")
  }

  run(blocks, genesis, reward, 0)



}
