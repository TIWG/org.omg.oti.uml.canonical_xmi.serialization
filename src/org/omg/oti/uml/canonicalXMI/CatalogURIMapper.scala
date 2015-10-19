/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.uml.canonicalXMI

import org.omg.oti.uml.UMLError

import java.net.{URI, URL}

import org.omg.oti.uml.read.api.UML

import scala.{Boolean, Int, Option, None, Some, StringContext, Unit}
import scala.Predef.{classOf,require, String}
import scala.collection.immutable._
import scala.util.control.Exception._

import scalaz._, Scalaz._, syntax.std._

import org.apache.xml.resolver.Catalog
import org.apache.xml.resolver.CatalogManager
import org.apache.xml.resolver.tools.CatalogResolver

class CatalogURIMapperException
(override val message: String,
 override val cause: Option[java.lang.Throwable] = None)
  extends UMLError.UException(message, cause)

case class CatalogURIMapper(
                             catalogManager: CatalogManager,
                             catalogResolver: CatalogResolver,
                             catalog: Catalog) {

  def this(catalogManager: CatalogManager,
           catalogResolver: CatalogResolver) =
    this(catalogManager, catalogResolver, catalogResolver.getCatalog)

  def this(catalogManager: CatalogManager) =
    this(catalogManager, new CatalogResolver(catalogManager))

  def parseCatalog(catalogURI: URI)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    catching(classOf[java.io.IOException])
    .withApply{ cause: java.lang.Throwable =>
      NonEmptyList(catalogURIMapperException(s"failed to parse catalog: $catalogURI", cause.some)).left
    }
    .apply(catalog.parseCatalog(catalogURI.toURL).right)

  def loadResolutionStrategy
  (appendDocumentExtensionUnlessPresent: Option[String])
  (resolved: String)
  : NonEmptyList[java.lang.Throwable] \/ Option[URI] =

    if (!appendDocumentExtensionUnlessPresent.getOrElse(".").startsWith("."))
      NonEmptyList(
        catalogURIMapperException(
          "The document extension, when specified, must start with '.'",
          new java.lang.IllegalArgumentException(
            s"Illegal value for appendDocumentExtensionUnlessPresent: " +
              s"'$appendDocumentExtensionUnlessPresent'").some)).left

    else
      catching(classOf[java.io.IOException])
      .withApply { cause: java.lang.Throwable =>
        NonEmptyList(
          catalogURIMapperException(s"failed to parse '$resolved' as a URL", cause.some)).left
      }
      .apply({
        val normalized = new URI(resolved)
        val normalizedPath = normalized.toString

        val f1 = new URL(normalizedPath)
        val f2 = appendDocumentExtensionUnlessPresent.fold[URL](f1) { ext =>
          if ( normalizedPath.endsWith( ext ) )
            f1
          else
            new URL( normalizedPath + ext )
        }
        loadResolutionStrategyPair(f1, f2)
      })

  def loadResolutionStrategyPair
  (f1: URL, f2: URL)
  : NonEmptyList[java.lang.Throwable] \/ Option[URI] =
    catching(
      classOf[java.io.IOException],
      classOf[java.lang.SecurityException],
      classOf[java.net.URISyntaxException])
    .withApply { _: java.lang.Throwable =>
      Option.empty[URI].right
    }
    .apply({
      val r1: Option[URI] =
        for {
          is <- Option.apply(f2.openStream)
          if is.available() > 0
        } yield {
          is.close()
          f2.toURI
        }

      r1
      .fold[NonEmptyList[java.lang.Throwable] \/ Option[URI]](
          catching(
            classOf[java.io.IOException],
            classOf[java.lang.SecurityException])
          .withApply { _: java.lang.Throwable =>
            Option.empty[URI].right
          }
          .apply({
            val r2: Option[URI] = for {
              is <- Option.apply(
                f1.openStream()
              )
              if is.available() > 0
            } yield {
              is.close()
              f1.toURI
            }
            r2
              .fold[NonEmptyList[java.lang.Throwable] \/ Option[URI]](
              Option.empty[URI].right
            ) { uri: URI =>
              uri.some.right
            }
          })
        ) { uri: URI =>
          uri.some.right
        }
    })

  def saveResolutionStrategy(resolved: String)
  : NonEmptyList[java.lang.Throwable] \/ Option[URI] =
    catching(
      classOf[java.io.IOException],
      classOf[java.lang.SecurityException],
      classOf[java.net.URISyntaxException],
      classOf[java.net.MalformedURLException],
      classOf[java.lang.NullPointerException])
  .withApply {
    (cause: java.lang.Throwable) =>
      NonEmptyList(
        catalogURIMapperException(
          s"saveResolutionStrategy: resolved=$resolved failed: ${cause.getMessage}",
          cause.some)
      ).left
  }
  .apply({
    val normalized = new URI(resolved)
    val normalizedPath = normalized.toString
    val f1 = new URL(normalizedPath)
    val outputFile =
      if (resolved.startsWith("file:"))
        new java.io.File(resolved.substring(5))
      else
        new java.io.File(resolved)
    Option.apply(outputFile.getParentFile)
    .fold[Option[URI]](Option.empty[URI]) { outputDir =>
      if (!outputDir.exists) {
        outputDir.mkdirs
      }
      if (outputDir.exists && outputDir.isDirectory && outputDir.canWrite)
        f1.toURI.some
      else
        Option.empty[URI]
    }
    .right
  })

  def resolve(uri: String)
  : NonEmptyList[java.lang.Throwable] \/ Option[String] =
    catching(classOf[java.net.MalformedURLException])
    .withApply { cause: java.lang.Throwable =>
      NonEmptyList(
        catalogURIMapperException(s"resolve(uri=$uri) failed: ${cause.getMessage}", cause.some)
      ).left
    }
    .apply(
      Option.apply(
        catalog.resolveURI(uri)
      ).right
    )

  def resolveURI
  ( uri: URI,
    resolutionStrategy: (String) => NonEmptyList[java.lang.Throwable] \/ Option[URI])
  : NonEmptyList[java.lang.Throwable] \/ Option[URI] = {

    val rawPath = uri.toString
    val iriPath =
      if (rawPath.endsWith("#"))
        rawPath.substring(0, rawPath.length() - 1)
      else
        rawPath
    catching(
      classOf[java.net.MalformedURLException],
      classOf[java.io.IOException])
    .withApply { t: java.lang.Throwable =>
      NonEmptyList(
        catalogURIMapperException(s"resolveURI(uri=$uri) failed", t.some)
      ).left
    }
    .apply(
      resolve(iriPath)
      .flatMap {
        _.fold[NonEmptyList[java.lang.Throwable] \/ Option[URI]](
          Option.empty[URI].right
        ) { resolved =>
          resolutionStrategy(resolved)
        }
      })
  }
}

object CatalogURIMapper {

  type CatalogURIMapperAdd = (CatalogURIMapper, => CatalogURIMapper) => CatalogURIMapper

  /**
   * Creates a CatalogURIMapper from Catalog files.
   *
   * @param catalogFiles
   * @param verbosity
   * @return
   */
  def createMapperFromCatalogFiles
  ( catalogFiles: Seq[java.io.File],
    verbosity: Int = 0)
  : NonEmptyList[java.lang.Throwable] \/ CatalogURIMapper = {
    val catalog = new CatalogManager()
    catalog.setUseStaticCatalog(false)
    catalog.setRelativeCatalogs(true)
    catalog.setVerbosity(verbosity)
    val mapper = new CatalogURIMapper(catalog)
    val c0: NonEmptyList[java.lang.Throwable] \/ CatalogURIMapper = mapper.right
    val cN: NonEmptyList[java.lang.Throwable] \/ CatalogURIMapper =
      ( c0 /: catalogFiles) { ( ci, catalogFile ) =>
      if (!catalogFile.exists)
        ci +++
          NonEmptyList(
            catalogURIMapperException(
              s"createMapperFromCatalogFiles failed",
              new java.io.FileNotFoundException(catalogFile.getAbsolutePath).some)).left
      else
        ci +++ mapper.parseCatalog(catalogFile.toURI).map( _ => mapper)
    }

    cN
  }

  /**
   * Creates a CatalogURIMapper from Catalog file URIs
   *
   * @param catalogURIs URIs of the Catalog files
   * @param verbosity
   * @return
   */
  def createMapperFromCatalogURIs
  ( catalogURIs: Seq[URI],
    verbosity: Int = 0)
  : NonEmptyList[java.lang.Throwable] \/ CatalogURIMapper = {
    val catalog = new CatalogManager()
    catalog.setUseStaticCatalog(false)
    catalog.setRelativeCatalogs(true)
    catalog.setVerbosity(verbosity)
    val mapper = new CatalogURIMapper(catalog)
    val c0: NonEmptyList[java.lang.Throwable] \/ CatalogURIMapper = mapper.right
    val cN: NonEmptyList[java.lang.Throwable] \/ CatalogURIMapper =
      ( c0 /: catalogURIs) { ( ci, catalogURI ) =>
      ci +++ mapper.parseCatalog(catalogURI).map( _ => mapper)
    }

    cN
  }


  // @todo is this a semigroup? if not, then what's a better way of achieving the desired result?

  implicit def CatalogURIMapperSemigroup: Semigroup[CatalogURIMapper] =
    new Semigroup[CatalogURIMapper] {
      def append(x: CatalogURIMapper, y: => CatalogURIMapper): CatalogURIMapper = x
    }

}