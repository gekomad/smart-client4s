val http4sVersion = "1.0.0-M44"
val circeVersion  = "0.14.16"

lazy val root = project
  .in(file("."))
  .settings(
    name              := "smart-client4s",
    version           := "0.1.1",
    scalaVersion      := "3.8.4",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafmtOnCompile := true,
    organization      := "com.github.gekomad",
    libraryDependencies ++= Seq(
      "org.http4s"                   %% "http4s-circe"           % http4sVersion,
      "org.http4s"                   %% "http4s-dsl"             % http4sVersion,
      "org.http4s"                   %% "http4s-jdk-http-client" % "1.0.0-M10",
      "io.circe"                     %% "circe-generic"          % circeVersion,
      "io.circe"                     %% "circe-parser"           % circeVersion,
      "com.github.cb372"             %% "cats-retry"             % "4.0.0",
      "org.typelevel"                %% "log4cats-slf4j"         % "2.8.0",
      "com.github.ben-manes.caffeine" % "caffeine"               % "3.2.4",
      "ch.qos.logback"                % "logback-classic"        % "1.5.38",
      "org.http4s"                   %% "http4s-ember-server"    % http4sVersion % Test,
      "org.typelevel"                %% "munit-cats-effect"      % "2.2.0"       % Test
    )
  )

scalacOptions ++= Seq(
  "-Xmax-inlines",
  "300",
  "-language:postfixOps",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-old-syntax",
  "-source",
  "3.7-migration",
  "-rewrite",
  "-Werror",
  "-Wvalue-discard",
  "-Wunused:all",
  "-Wnonunit-statement"
)

testFrameworks += new TestFramework("munit.Framework")
Test / parallelExecution := false
