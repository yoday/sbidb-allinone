name := "sbidb-allinone"

version := "0.1"

scalaVersion := "2.12.15"

// Web Tech
//libraryDependencies += "io.d11"                            % "zhttp_2.12"                   % "2.0.0-RC4"  // ZIO v2
//libraryDependencies += "dev.zio"                           % "zio-json_2.12"                % "0.3.0-RC3"  // ZIO v2
libraryDependencies += "io.d11"                            % "zhttp_2.12"                   % "1.0.0.0-RC25" // ZIO v1
libraryDependencies += "dev.zio"                           % "zio-json_2.12"                % "0.2.0-M1"     // ZIO v1
libraryDependencies += "com.github.spullara.mustache.java" % "compiler"                     % "0.9.10"

// SQL Tech
libraryDependencies += "io.zonky.test"                     % "embedded-postgres"            % "1.3.1"
libraryDependencies += "com.typesafe.slick"                % "slick_2.12"                   % "3.4.0-M1"
libraryDependencies += "com.github.tminglei"               % "slick-pg_2.12"                % "0.20.2"

// Search Tech
libraryDependencies += "pl.allegro.tech"                   % "embedded-elasticsearch"       % "2.10.0"
//libraryDependencies += "com.sksamuel.elastic4s"            % "elastic4s-client-sttp_2.12" % "7.17.2" // ES 7.x
libraryDependencies += "com.sksamuel.elastic4s"            % "elastic4s-sttp_2.12" % "6.7.8" // ES 6.x

// Utilities
libraryDependencies += "org.slf4j"                         % "slf4j-reload4j"               % "1.7.36"
libraryDependencies += "info.debatty"                      % "java-string-similarity"       % "2.0.0"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:higherKinds",
//  "-language:implicitConversions",
//  "-language:existentials",
//  "-language:postfixOps",
)
