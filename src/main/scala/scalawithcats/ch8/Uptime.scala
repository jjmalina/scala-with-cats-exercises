package scalawithcats.ch8

import cats.Id
import cats.Applicative
import cats.syntax.functor._ // for map
import cats.instances.list._
import cats.syntax.traverse._
import scala.concurrent.Future

trait UptimeClient[F[_]] {
  def getUptime(hosname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hosname: String): Future[Int]
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}

object UptimeTestSuite {
  def testTotalUpTime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
}

object UptimeApp extends App {
  import UptimeTestSuite._
  testTotalUpTime()
}
