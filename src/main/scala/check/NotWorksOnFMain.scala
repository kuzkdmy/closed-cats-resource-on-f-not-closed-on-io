package check

import cats.Monad
import cats.effect.{Blocker, Bracket, ExitCode, IO, IOApp, Resource}
import distage.{Injector, ModuleDef, Roots}
import izumi.distage.model.Locator
import izumi.distage.model.definition.DIResource

object NotWorksOnFMain extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    distageAppResource.use(_ => IO.never).as(ExitCode.Success)
  }
  def distageAppResource: DIResource.DIResourceBase[IO, Locator] = {
    val plan = Injector().plan(
      Seq(distageDBModule).merge,
      Roots.Everything
    )
    Injector().produceF[IO](plan)
  }

  val distageDBModule: ModuleDef = new ModuleDef {
    addImplicit[Bracket[IO, Throwable]]

    make[(ApplicationDB, Blocker)].fromResource(catsDBResource)
    make[ApplicationDB].from { db: (ApplicationDB, Blocker) => db._1 }
    make[Blocker].from { db: (ApplicationDB, Blocker) => db._2 }
  }

  case class ApplicationDB(poolName: String)
  def catsDBResource: Resource[IO, (ApplicationDB, Blocker)] = {
    new DBComponent[IO].resource
  }

  case class DBComponent[F[_]: Monad]() {
    import cats.implicits._
    def resource: Resource[F, (ApplicationDB, Blocker)] = {
      for {
        db <- Resource.make(
          for {
            _  <- println("init app db").pure[F]
            db <- ApplicationDB("appDB").pure[F]
          } yield db
        )(_ => println("close app db").pure[F])
      } yield (db, Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.Implicits.global))
    }
  }
}
