/*
 * Copyright 2014 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Copyright 2015 Airbus.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package org.omg.oti.uml.canonicalXMI

import java.io.{FileOutputStream, OutputStreamWriter, PrintWriter}

import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.uml._
import org.omg.oti.uml.read.UMLStereotypeTagValue
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi._

import scala.{Boolean, Function0, Option, None, Some, StringContext, Tuple2, Tuple3}
import scala.Predef.{Set => _, Map => _, _}
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.util.control.Exception._
import scalaz._, Free._, Scalaz._

class ResolvedDocumentSetException[Uml <: UML]
(rds: ResolvedDocumentSet[Uml],
 override val message: String,
 override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException(message, cause) {

  /**
    * This type member is intended to facilitate pattern matching
    * using a wildcard for the type parameter, i.e., ResolvedDocumentSetException[_]
    * The type information can then be checked using the UmlType member.
    */
  type UmlType = Uml
}

/**
  * Information about the result of resolving a [[DocumentSet]] as a graph of `Document` nodes
  * and inter-document edges.
  *
  * @param ds Information about the set of `Document` in scope of the resolution process
  * @param g The resolved directed graph of `Document` nodes & edges. This directed graph may have cycles!
  * @param unresolvedElementMapper A partial function for mapping unresolved `UMLElement` to a resolvable `UMLElement`
  * @tparam Uml The type signature for a tool-specific adaptation of the OTI UML API
  */
case class ResolvedDocumentSet[Uml <: UML]
(ds: DocumentSet[Uml],
 g: DocumentSet[Uml]#MutableDocumentSetGraph,
 unresolvedElementMapper: UMLElement[Uml] => Option[UMLElement[Uml]]) {

  implicit val dOps = ds.documentOps

  def isElementMapped2Document
  (e: UMLElement[Uml])
  : Boolean
  = element2mappedDocument(e).isDefined

  def element2mappedDocument
  (e: UMLElement[Uml]): Option[Document[Uml]]
  = ds.lookupDocumentByExtent(e)

  def getStereotype_ID_UUID
  (s: UMLStereotype[Uml])
  (implicit idg: IDGenerator[Uml]) 
  : Set[java.lang.Throwable] \/ (String @@ OTI_ID, String @@ OTI_UUID)
  = s
    .xmiID()
    .flatMap { _id =>

      val id_uuid
      : Set[java.lang.Throwable] \/ (String @@ OTI_ID, String @@ OTI_UUID)
      = s
        .xmiUUID()
        .flatMap { _uuid =>

          element2mappedDocument(s)
            .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID, String @@ OTI_UUID)](
            -\/(
              Set(
                resolvedDocumentSetException(
                  this,
                  "getStereotype_ID_UUID failed",
                  UMLError
                    .illegalElementError[Uml, UMLStereotype[Uml]](
                    s"There should be a document for stereotype ${s.qualifiedName.get} (ID=${_id})",
                    Iterable(s)))))
          ) {
            case d: Document[Uml] with BuiltInDocument =>
              dOps
                .getExternalDocumentURL(d.documentURL)
                .flatMap { url =>
                  catching(
                    classOf[java.lang.NullPointerException],
                    classOf[java.lang.IllegalArgumentException])
                    .withApply {
                      (cause: java.lang.Throwable) =>
                        -\/(
                          Set(
                            resolvedDocumentSetException(
                              this,
                              "getStereotype_ID_UUID failed",
                              UMLError
                                .illegalElementError[Uml, UMLStereotype[Uml]](
                                s"There should be a document for stereotype ${s.qualifiedName.get} (ID=${_id})",
                                Iterable(s)))))
                    }
                    .apply({
                      val sToolID = s.toolSpecific_id
                      val builtInURI = url.resolve("#" + sToolID).toString
                      ds.builtInURIMapper.resolve(builtInURI)
                        .map { oresolved =>
                          val mappedURI = oresolved.getOrElse(builtInURI)
                          val fragmentIndex = mappedURI.lastIndexOf('#')
                          require(fragmentIndex > 0)
                          val fragment = IDGenerator.xmlSafeID(mappedURI.substring(fragmentIndex + 1))
                          Tuple2(OTI_ID(fragment), OTI_UUID(OTI_NS_PREFIX.unwrap(d.info.nsPrefix) + fragment))
                        }
                    })
                }

            case d: Document[Uml] with SerializableDocument =>
              \/-(Tuple2(_id, _uuid))

            case d: Document[Uml] =>
              -\/(
                Set(
                  resolvedDocumentSetException(
                    this,
                    "getStereotype_ID_UUID failed",
                    UMLError
                      .illegalElementError[Uml, UMLStereotype[Uml]](
                      s"Unrecognized document $d for stereotype ${s.qualifiedName.get} (ID=${_id})",
                      Iterable(s)))))
          }
        }

      id_uuid
    }

  def serialize
  ()
  (implicit idg: IDGenerator[Uml])
  : Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)]
  = {

    val s0: Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)] = Set().right
    val sN: Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)] = (s0 /: g.nodes) {
      (si, n) => n.value match {
        case _: Document[Uml] with BuiltInDocument =>
          si
        case d: Document[Uml] with SerializableDocument =>
          si +++ serialize(d)
      }
    }

    sN
  }

  def serializePkg
  (pkg: UMLPackage[Uml])
  (implicit idg: IDGenerator[Uml])
  : Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)]
  = ds.lookupDocumentByScope(pkg)
      .fold[Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)]] {
      Set(
        resolvedDocumentSetException(
          this,
          "serializePkg failed",
          UMLError
            .illegalElementError[Uml, UMLPackage[Uml]](
            s"Serialization failed: no document found for ${pkg.qualifiedName.get}",
            Iterable(pkg))))
        .left
    } { d =>
      serialize(d)
    }

  protected def foldTagValues
  (xmiScopes: scala.xml.NamespaceBinding, idg: IDGenerator[Uml])
  (tagValueNodes: Set[java.lang.Throwable] \/ Vector[scala.xml.Elem],
   stereotypeTagValue: UMLStereotypeTagValue[Uml])
  : Set[java.lang.Throwable] \/ Vector[scala.xml.Elem]
  = tagValueNodes +++ stereotypeTagValue.serialize(xmiScopes, idg).map(_.to[Vector])

  protected def serialize
  (d: Document[Uml])
  (implicit idg: IDGenerator[Uml])
  : Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)]
  = \/.fromTryCatchNonFatal(new java.net.URI(OTI_URI.unwrap(d.info.packageURI)))
      .fold[Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)]](
    l = (t: java.lang.Throwable) =>
      -\/(
        Set[java.lang.Throwable](
          resolvedDocumentSetException(
            this,
            s"serialize failed: Cannot serialize document ${OTI_URI.unwrap(d.info.packageURI)}",
            t))),
    r = (duri: java.net.URI) =>
      ds
      .documentURIMapper
      .resolveURI(duri, ds.documentURIMapper.saveResolutionStrategy)
      .flatMap { ruri =>

        val uri = ruri.getOrElse(duri)
        val result: Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)] =
          \/.fromTryCatchNonFatal(new java.io.File(uri)) match {
            case -\/(t) =>
              -\/(
                Set[java.lang.Throwable](
                  resolvedDocumentSetException(
                    this,
                    s"serialize failed: Cannot serialize document "
                      + s"$duri mapped for save to $ruri: ${t.getMessage}",
                    t)))
            case \/-(furi) =>
              val s = d.scope.xmiID.flatMap { d_id =>
                d.scope.xmiUUID.flatMap { d_uuid =>
                  serialize(d, d_id, d_uuid, furi)
                }
              }
              s
          }

        result
      }
    )

  protected def serialize
  (d: Document[Uml],
   d_id: String @@ OTI_ID,
   d_uuid: String @@ OTI_UUID,
   furi: java.io.File)
  (implicit idg: IDGenerator[Uml])
  : Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)]
  = {

    val dir = furi.getParentFile
    dir.mkdirs()

    val tv0: Set[java.lang.Throwable] \/ Set[(UMLElement[Uml], Seq[UMLStereotypeTagValue[Uml]])] =
      Set().right
    val tvN: Set[java.lang.Throwable] \/ Set[(UMLElement[Uml], Seq[UMLStereotypeTagValue[Uml]])] =
      (tv0 /: d.extent) { (tvi, e) =>
        tvi +++
          e.tagValues
            .map(etvs => Set((e, etvs)))
      }

    tvN.flatMap { element2stereotypeTagValues =>

      val referencedProfiles: Set[UMLProfile[Uml]] =
        element2stereotypeTagValues.flatMap {
          case (_, stereotypeTagValues) =>
            stereotypeTagValues
              .to[Set]
              .flatMap(_.appliedStereotype.profile.filter(isElementMapped2Document))
        }

      val profiles
      : Vector[UMLProfile[Uml]]
      = referencedProfiles.to[Vector].sortBy(_.qualifiedName.get)

      serialize(d, d_id, d_uuid, furi, element2stereotypeTagValues.toMap, profiles)
    }
  }

  protected def serialize
  (d: Document[Uml],
   d_id: String @@ OTI_ID,
   d_uuid: String @@ OTI_UUID,
   furi: java.io.File,
   element2stereotypeTagValues: Map[UMLElement[Uml], Seq[UMLStereotypeTagValue[Uml]]],
   referencedProfiles: Vector[UMLProfile[Uml]])
  (implicit idg: IDGenerator[Uml])
  : Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)]
  = {
    import DocumentSet._
    import scala.xml._

    val s0: Set[java.lang.Throwable] \/ NamespaceBinding =
      \/-(null)
    val sN: Set[java.lang.Throwable] \/ NamespaceBinding =
      (s0 /: referencedProfiles) { (si, rP) =>
        (si |@| rP.getEffectiveURI()(idg.otiCharacteristicsProvider)) { (_si, _uri) =>
          _uri.fold(_si) { ns_uri =>
            NamespaceBinding(rP.name.get, OTI_URI.unwrap(ns_uri), _si)
          }
        }
      }

    sN.flatMap { profileScopes =>
      val xmiScopes =
        NamespaceBinding("xmi", XMI_ns,
          NamespaceBinding("xsi", XSI_ns,
            NamespaceBinding("uml", UML_ns,
              NamespaceBinding("mofext", MOFEXT_ns, profileScopes))))
      serialize(d, d_id, d_uuid, furi, element2stereotypeTagValues, referencedProfiles, xmiScopes)
    }
  }

  protected def serialize
  (d: Document[Uml],
   d_id: String @@ OTI_ID,
   d_uuid: String @@ OTI_UUID,
   furi: java.io.File,
   element2stereotypeTagValues: Map[UMLElement[Uml], Seq[UMLStereotypeTagValue[Uml]]],
   referencedProfiles: Vector[UMLProfile[Uml]],
   xmiScopes: scala.xml.NamespaceBinding)
  (implicit idg: IDGenerator[Uml])
  : Set[java.lang.Throwable] \/ Set[(Document[Uml], java.io.File)] = {

    val elementOrdering = scala.collection.mutable.ArrayBuffer[UMLElement[Uml]]()

    val free: Free[Function0, Set[java.lang.Throwable] \/ scala.xml.Node] =
      generateNodeElement(
        elementOrdering,
        d, "uml", d.scope.xmiElementLabel,
        d.scope, xmiScopes)

    val result: Set[java.lang.Throwable] \/ scala.xml.Node =
      free.go(f => Comonad[Function0].copoint(f))(Applicative[Function0])

    // alternatively:
    // val result = free.run

    result
      .flatMap { top =>
        val mofTagRef: scala.xml.MetaData =
          new scala.xml.PrefixedAttribute(pre = "xmi", key = "idref", value = OTI_ID.unwrap(d_id), scala.xml.Null)
        val mofTagElement: scala.xml.Node =
          scala.xml.Elem(
            prefix = null, label = "element", attributes = mofTagRef,
            scope = xmiScopes, minimizeEmpty = true)
        val mofTag = scala.xml.Elem(
          prefix = "mofext",
          label = "Tag",
          attributes =
            new scala.xml.PrefixedAttribute(
              pre = "xmi", key = "id", value = d_id + "_mofext.Tag",
              new scala.xml.PrefixedAttribute(
                pre = "xmi", key = "uuid", value = d_uuid + "_mofext.Tag",
                new scala.xml.PrefixedAttribute(
                  pre = "xmi", key = "type", value = "mofext:Tag",
                  d.scope match {
                    case ne: UMLNamedElement[Uml] =>
                      ne
                        .name
                        .fold[scala.xml.MetaData](scala.xml.Null) { name =>
                        new scala.xml.UnprefixedAttribute(
                          key = "name", value = "org.omg.xmi.nsPrefix",
                          new scala.xml.UnprefixedAttribute(
                            key = "value", value = name,
                            scala.xml.Null))
                      }
                    case _ =>
                      scala.xml.Null
                  }))),
          scope = xmiScopes,
          minimizeEmpty = true,
          mofTagElement)

        val sTV0
        : Set[java.lang.Throwable] \/ Vector[scala.xml.Node]
        = Vector[scala.xml.Node]().right

        val sTVN
        : Set[java.lang.Throwable] \/ Vector[scala.xml.Node]
        = (sTV0 /: elementOrdering.to[Vector]) { (sTVi, e) =>
          e
          .xmiID()
          .flatMap { eID =>
            e
            .xmiUUID()
            .flatMap { eUUID =>

              val allTagValuesByStereotype: Map[UMLStereotype[Uml], Seq[UMLStereotypeTagValue[Uml]]] =
                element2stereotypeTagValues
                  .get(e)
                  .fold[Map[UMLStereotype[Uml], Seq[UMLStereotypeTagValue[Uml]]]](Map()) { tagValues =>
                  tagValues.groupBy(_.appliedStereotype)
                }

              val appliedStereotypes: Set[UMLStereotype[Uml]] =
                element2stereotypeTagValues
                  .get(e)
                  .fold[Set[UMLStereotype[Uml]]](Set()) { tagValues =>
                  tagValues.map(_.appliedStereotype).toSet filter isElementMapped2Document
                }

              val ordering
              : Vector[UMLStereotype[Uml]]
              = appliedStereotypes
                  .to[Vector]
                  .sortBy(
                    getStereotype_ID_UUID(_)
                    .getOrElse(Tuple2(OTI_ID(""), OTI_UUID(""))) // @todo propagate errors
                    match { case (id, uuid) => OTI_ID.unwrap(id) + OTI_UUID.unwrap(uuid) }
                  )


              val oTVE0
              : Set[java.lang.Throwable] \/ Vector[scala.xml.Node]
              = Vector[scala.xml.Node]().right

              val oTVEN
              : Set[java.lang.Throwable] \/ Vector[scala.xml.Node]
              = (oTVE0 /: ordering) { (oTVEi, s) =>
                getStereotype_ID_UUID(s)
                  .flatMap {
                    case (sID, sUUID) =>
                      val tagValueAttributes
                      : Set[java.lang.Throwable] \/ Vector[scala.xml.Elem]
                      = allTagValuesByStereotype
                          .get(s)
                          .fold[Set[java.lang.Throwable] \/ Vector[scala.xml.Elem]](
                              Vector[scala.xml.Elem]().right
                          ) { vs =>
                          val tagValueAttribute0
                          : Set[java.lang.Throwable] \/ Vector[scala.xml.Elem]
                          = Vector[scala.xml.Elem]().right
                          val tagValueAttributeN = (tagValueAttribute0 /: vs) (foldTagValues(xmiScopes, idg))
                          tagValueAttributeN
                        }
                      val stAppID = IDGenerator.computeStereotypeApplicationOTI_ID(eID, sID)
                      val stAppUUID = IDGenerator.computeStereotypeApplicationOTI_UUID(eUUID, sUUID)
                      val xmiTagValueAttributes =
                        new scala.xml.PrefixedAttribute(
                          pre = "xmi", key = "id", value = OTI_ID.unwrap(stAppID),
                          new scala.xml.PrefixedAttribute(
                            pre = "xmi", key = "uuid", value = OTI_UUID.unwrap(stAppUUID),
                            new scala.xml.PrefixedAttribute(
                              pre = "xmi", key = "type", value = s.profile.get.name.get + ":" + s.name.get,
                              scala.xml.Null)))

                      tagValueAttributes.map { tVAs =>
                        Vector(scala.xml.Elem(
                          prefix = s.profile.get.name.get,
                          label = s.name.get,
                          attributes = xmiTagValueAttributes,
                          scope = xmiScopes,
                          minimizeEmpty = true,
                          tVAs: _*))
                      }
                  }
              }

              sTVi +++ oTVEN

            }
          }
      }

        sTVN
          .flatMap { stereotypeTagValues =>

            val xmi = scala.xml.Elem(
              prefix = "xmi",
              label = "XMI",
              attributes = scala.xml.Null,
              scope = xmiScopes,
              minimizeEmpty = true,
              top +: mofTag +: stereotypeTagValues: _*)

            val filepath = furi.getPath + ".xmi"
            \/.fromTryCatchNonFatal[java.io.File]({
              val xmlFile = new java.io.File(filepath)
              val xmlPrettyPrinter = new scala.xml.PrettyPrinter(width = 300, step = 2)
              val xmlOutput = xmlPrettyPrinter.format(xmi)
              val bw = new PrintWriter(new OutputStreamWriter(new FileOutputStream(xmlFile), "UTF-8"))
              bw.println("<?xml version='1.0' encoding='UTF-8'?>")
              bw.println(xmlOutput)
              bw.close()
              xmlFile
            }) match {
              case -\/(t) =>
                Set[java.lang.Throwable](
                  resolvedDocumentSetException(
                    this,
                    s"serialize failed: Cannot save XMI serialization "
                      + s"${d.info.packageURI} to file: $filepath: ${t.getMessage}",
                    t)).left
              case \/-(file) =>
                Set[(Document[Uml], java.io.File)]((d, file)).right
            }
          }
      }
  }

  type MetaPropertyFunctionSet = Set[_ <: MetaPropertyFunction[Uml, _ <: UMLElement[Uml], _ <: UMLElement[Uml]]]
  type UMLElementSet = Set[UMLElement[Uml]]

  import scala.xml.NodeSeq

  type SerializationState = (UMLElementSet, NodeSeq, MetaPropertyFunctionSet)

  @tailrec final protected def append1Pair
  (sub: UMLElement[Uml],
   t: Trampoline[Set[java.lang.Throwable] \/ scala.xml.Node],
   subElements: Set[UMLElement[Uml]],
   nodes: Seq[scala.xml.Node],
   redefinitions: MetaPropertyFunctionSet)
  : Trampoline[Set[java.lang.Throwable] \/ SerializationState]
  = {

    //    assert( Thread.currentThread().getStackTrace.count( _.getMethodName == "append1Node" ) == 1,
    //      "Verification that the trampolined recursive function 'append1Node' runs stack-free" )

    t.resume match {
      //case -\/( s ) => suspend { append1Node( nodes, s() ) }
      case -\/(s) =>
        append1Pair(sub, s(), subElements, nodes, redefinitions)
      case \/-(r) =>
        r match {
          case -\/(f) =>
            return_ {
              -\/(f)
            }
          case \/-(n) =>
            return_ {
              val result: SerializationState = (subElements + sub, nodes :+ n, redefinitions)
              \/-(result)
            }
        }
    }
  }

  @tailrec final protected def prependNestedElement
  (sub: UMLElement[Uml],
   t: Trampoline[Set[java.lang.Throwable] \/ scala.xml.Node],
   subElements: Set[UMLElement[Uml]],
   nodes: Seq[scala.xml.Node],
   redefinitions: MetaPropertyFunctionSet)
  : Trampoline[Set[java.lang.Throwable] \/ SerializationState]
  = {

    //    assert( Thread.currentThread().getStackTrace.count( _.getMethodName == "append1Node" ) == 1,
    //      "Verification that the trampolined recursive function 'append1Node' runs stack-free" )

    t.resume match {
      //case -\/( s ) => suspend { append1Node( nodes, s() ) }
      case -\/(s) =>
        prependNestedElement(sub, s(), subElements, nodes, redefinitions)
      case \/-(r) => r match {
        case -\/(f) =>
          return_ {
            f.left
          }
        case \/-(n) =>
          return_ {
            val result: SerializationState = (subElements + sub, n +: nodes, redefinitions)
            result.right
          }
      }
    }
  }


  @tailrec final protected def append1Node
  (nodes: NodeSeq,
   t: Trampoline[Set[java.lang.Throwable] \/ scala.xml.Node])
  : Trampoline[Set[java.lang.Throwable] \/ NodeSeq] = {

    //    assert( Thread.currentThread().getStackTrace.count( _.getMethodName == "append1Node" ) == 1,
    //      "Verification that the trampolined recursive function 'append1Node' runs stack-free" )

    t.resume match {
      //case -\/( s ) => suspend { append1Node( nodes, s() ) }
      case -\/(s) =>
        append1Node(nodes, s())
      case \/-(r) => r match {
        case -\/(f) => return_ {
          f.left
        }
        case \/-(n) => return_ {
          (nodes :+ n).right
        }
      }
    }
  }

  @tailrec final protected def appendNodes
  (t1: Trampoline[Set[java.lang.Throwable] \/ NodeSeq],
   t2: Trampoline[Set[java.lang.Throwable] \/ scala.xml.Node])
  : Trampoline[Set[java.lang.Throwable] \/ NodeSeq] = {

    //    assert(
    //      Thread.currentThread().getStackTrace.count( _.getMethodName == "appendNodes" ) == 1,
    //      "Verification that the trampolined recursive function 'appendNodes' runs stack-free" )

    t1.resume match {
      //case -\/( s )             => suspend { appendNodes( s(), t2 ) }
      case -\/(s) =>
        appendNodes(s(), t2)
      case \/-(-\/(f)) => return_ {
        f.left
      }
      case \/-(\/-(ns)) =>
        suspend {
          append1Node(ns, t2)
        }
    }
  }

  @tailrec final protected def wrapNodes
  (xRefAttrs: Set[java.lang.Throwable] \/ NodeSeq,
   t: Trampoline[Set[java.lang.Throwable] \/ SerializationState],
   xRefRefs: Set[java.lang.Throwable] \/ NodeSeq,
   prefix: String,
   label: String,
   xmlAttributesAndLocalReferences: scala.xml.MetaData,
   xmiScopes: scala.xml.NamespaceBinding)
  : Trampoline[Set[java.lang.Throwable] \/ scala.xml.Node] = {

    //    assert( Thread.currentThread().getStackTrace.count( _.getMethodName == "wrapNodes" ) == 1,
    //      "Verification that the trampolined function 'wrapNodes' runs recursively stack-free" )

    t.resume match {
      //      case -\/( s ) =>
      //        suspend { wrapNodes( s(), prefix, label, xmlAttributesAndLocalReferences, xmiScopes ) }
      case -\/(s) =>
        wrapNodes(xRefAttrs, s(), xRefRefs, prefix, label, xmlAttributesAndLocalReferences, xmiScopes)
      case \/-(-\/(f)) =>
        return_ {
          f.left
        }
      case \/-(\/-((_, sNodes, _))) =>
        (xRefAttrs, xRefRefs) match {
          case (-\/(f), _) =>
            return_ {
              f.left
            }
          case (_, -\/(f)) =>
            return_ {
              f.left
            }
          case (\/-(aNodes), \/-(rNodes)) =>
            import scala.xml._
            val node = Elem(
              prefix = prefix,
              label = label,
              attributes = xmlAttributesAndLocalReferences,
              scope = xmiScopes,
              minimizeEmpty = true,
              aNodes ++ sNodes ++ rNodes: _*)
            return_ {
              node.right
            }
        }
    }
  }

  protected def generateNodeElement
  (elementOrdering: scala.collection.mutable.ArrayBuffer[UMLElement[Uml]],
   d: Document[Uml],
   prefix: String,
   label: String,
   e: UMLElement[Uml],
   xmiScopes: scala.xml.NamespaceBinding)
  (implicit idg: IDGenerator[Uml])
  : Trampoline[Set[java.lang.Throwable] \/ scala.xml.Node] = {

    elementOrdering += e

    //    assert(
    //      Thread.currentThread().getStackTrace.count( _.getMethodName == "generateNodeElement" ) == 1,
    //      s"Verification that the trampolined function 'wrapNodes' runs recursively " +
    //      s"stack-free for label=${label}, e=${e.xmiID}" )

    def foldAttribute
    (next: Set[java.lang.Throwable] \/ scala.xml.MetaData, f: e.MetaAttributeFunction)
    : Set[java.lang.Throwable] \/ scala.xml.MetaData 
    = {
      val value = f match {
        case e.id_metaDocumentAttributeFunction =>
          e.xmiID().map { id => Iterable(Tag.unwrap(id)) }
        case e.uuid_metaDocumentAttributeFunction =>
          e.xmiUUID().map { uuid => Iterable(Tag.unwrap(uuid)) }
        case _ =>
          f.evaluate(e, idg.otiCharacteristicsProvider)
      }
      (next, value) match {
        case (-\/(t), _) =>
          t.left
        case (_, -\/(t)) =>
          t.left
        case (\/-(n), \/-(values)) =>
          (n /: values) {
            case (_n, _value) =>
              f
              .attributePrefix
              .fold[scala.xml.Attribute] {
                new scala.xml.UnprefixedAttribute(key = f.attributeName, value = _value, _n)
              } { aPrefix =>
                new scala.xml.PrefixedAttribute(pre = aPrefix, key = f.attributeName, value = _value, _n)
              }
          }.right
      }
    }
    
    def foldAttributeNode
    (nodes: Set[java.lang.Throwable] \/ NodeSeq, f: e.MetaAttributeFunction)
    : Set[java.lang.Throwable] \/ NodeSeq =
      (nodes, f.evaluate(e, idg.otiCharacteristicsProvider)) match {
        case (-\/(t), _) =>
          t.left
        case (_, -\/(t)) =>
          t.left
        case (\/-(ns), \/-(values)) =>
          val valueNodes = for {
            value <- values
          } yield scala.xml.Elem(
            prefix = null, label = f.attributeName,
            attributes = scala.xml.Null, scope = xmiScopes, minimizeEmpty = true, scala.xml.Text(value))
          (ns ++ valueNodes).right
      }

    def foldReference
    (nodes: Set[java.lang.Throwable] \/ scala.xml.NodeSeq, f: e.MetaPropertyEvaluator)
    : Set[java.lang.Throwable] \/ scala.xml.NodeSeq =
      nodes.flatMap { ns: scala.xml.NodeSeq =>
        f match {
          case rf: e.MetaReferenceEvaluator =>
            rf
            .evaluate(e)
            .flatMap(
              _.fold[Set[java.lang.Throwable] \/ scala.xml.NodeSeq](
                ns.right
              ){ eRef =>
                eRef
                .xmiID()
                .flatMap { eRefID =>
                  element2mappedDocument(eRef)
                  .fold[Set[java.lang.Throwable] \/ scala.xml.NodeSeq](
                    ns.right
                  ){ dRef =>
                    if (d == dRef) {
                      val idrefAttrib: scala.xml.MetaData =
                        new scala.xml.PrefixedAttribute(pre = "xmi", key = "idref", value = OTI_ID.unwrap(eRefID), scala.xml.Null)
                      val idrefNode: scala.xml.Node =
                        scala.xml.Elem(
                          prefix = null, label = f.propertyName,
                          attributes = idrefAttrib, scope = xmiScopes, minimizeEmpty = true)
                      (ns :+ idrefNode).right
                    } else {
                      dOps
                      .getExternalDocumentURL(dRef.documentURL)
                      .flatMap { dURI =>
                        val oti_href = dURI.toString + "#" + eRefID
                        
                        val eRefToolID = eRef.toolSpecific_id
                        val tool_href = dURI.toString + "#" + eRefToolID
                        
                        val externalHRef
                        : Set[java.lang.Throwable] \/ String
                        = dRef match {
                          case _: Document[Uml] with SerializableDocument =>
                            oti_href.right
                          case _: Document[Uml] with BuiltInDocument =>
                            ds.builtInURIMapper.resolve(tool_href).map(_.getOrElse(tool_href))
                        }

                        externalHRef
                        .map { exhref =>
                          val hrefAttrib
                          : scala.xml.MetaData 
                          = new scala.xml.UnprefixedAttribute(key = "href", value = exhref, scala.xml.Null)
                        
                          val hrefNode
                          : scala.xml.Node 
                          = scala.xml.Elem(
                            prefix = null, label = f.propertyName, attributes = hrefAttrib,
                            scope = xmiScopes, minimizeEmpty = true)
                          
                          (ns :+ hrefNode)
                        }
                      }
                    }
                  }
                }
              })

          case cf: e.MetaCollectionEvaluator =>
            cf
              .evaluate(e)
              .flatMap { eRefs: Vector[UMLElement[Uml]] =>
                if (eRefs.isEmpty)
                  ns.right
                else {
                  val ordered_eRefs: Vector[UMLElement[Uml]] =
                    if (cf.isOrdered)
                      eRefs
                    else
                      eRefs.sortBy(_.xmiOrderingKey.getOrElse("")) // @todo propagate errors
                  val hRef0: Set[java.lang.Throwable] \/ scala.xml.NodeSeq = scala.xml.NodeSeq.Empty.right
                  val hRefN: Set[java.lang.Throwable] \/ scala.xml.NodeSeq = (hRef0 /: ordered_eRefs ) {
                    (hRefi, eRef) =>
                      eRef.xmiID()
                        .flatMap { eRefID =>
                          element2mappedDocument(eRef)
                            .fold[Set[java.lang.Throwable] \/ scala.xml.NodeSeq](
                            scala.xml.NodeSeq.Empty.right
                          ) { dRef =>
                            val dNodes: Set[java.lang.Throwable] \/ scala.xml.NodeSeq =
                              if (d == dRef) {
                                val idrefAttrib: scala.xml.MetaData =
                                  new scala.xml.PrefixedAttribute(pre = "xmi", key = "idref", value = OTI_ID.unwrap(eRefID), scala.xml.Null)
                                val idrefNode: scala.xml.Node =
                                  scala.xml.Elem(
                                    prefix = null, label = f.propertyName,
                                    attributes = idrefAttrib, scope = xmiScopes, minimizeEmpty = true)
                                \/-(idrefNode)
                              } else {
                                val oti_href = dRef.documentURL.toString + "#" + eRefID
                                
                                val eRefToolID = eRef.toolSpecific_id
                                val tool_href = dRef.documentURL.toString + "#" + eRefToolID
                        
                                val externalHRef: Set[java.lang.Throwable] \/ String =
                                  dRef match {
                                    case _: Document[Uml] with SerializableDocument =>
                                      oti_href.right
                                    case _: Document[Uml] with BuiltInDocument =>
                                      ds.builtInURIMapper.resolve(tool_href).map(_.getOrElse(tool_href))
                                  }

                                externalHRef
                                  .map { exhref =>
                                    val hrefAttrib: scala.xml.MetaData =
                                      new scala.xml.UnprefixedAttribute(key = "href", value = exhref, scala.xml.Null)
                                    val hrefNode: scala.xml.Node =
                                      scala.xml.Elem(
                                        prefix = null, label = f.propertyName, attributes = hrefAttrib,
                                        scope = xmiScopes, minimizeEmpty = true)
                                    hrefNode
                                  }
                              }
                            dNodes
                          }
                        }
                  }
                  hRefN.map { hRefs =>
                    ns ++ hRefs
                  }
                }
              }
        }
      }

    def waitGenerateNodeElement
    (f: e.MetaPropertyEvaluator,
     sub: UMLElement[Uml])
    : Set[java.lang.Throwable] \/ scala.xml.Node =
      callGenerateNodeElement(f, sub).run

    def callGenerateNodeElement
    (f: e.MetaPropertyEvaluator,
     sub: UMLElement[Uml])
    : Trampoline[Set[java.lang.Throwable] \/ scala.xml.Node] =
      generateNodeElement(elementOrdering, d, null, f.propertyName, sub, xmiScopes)

    def waitGenerateNodeReference
    (f: e.MetaPropertyEvaluator,
     sub: UMLElement[Uml])
    : Set[java.lang.Throwable] \/ scala.xml.Node = {
      val idRefAttrib: scala.xml.MetaData =
        new scala.xml.PrefixedAttribute(
          pre = "xmi",
          key = "idref",
          value = sub.xmiID().map(OTI_ID.unwrap).getOrElse(""), // @todo propagate errors
          scala.xml.Null)

      val idRefNode: scala.xml.Node = scala.xml.Elem(
        prefix = null,
        label = f.propertyName,
        attributes = idRefAttrib,
        scope = xmiScopes,
        minimizeEmpty = true)

      idRefNode.right
    }

    def callGenerateNodeReference
    (f: e.MetaPropertyEvaluator,
     sub: UMLElement[Uml])
    : Trampoline[Set[java.lang.Throwable] \/ scala.xml.Node] = {
      return_ {
        waitGenerateNodeReference(f, sub)
      }
    }

    def prependNestedElementsOrIdReferences
    (f: e.MetaPropertyEvaluator,
     subs: Vector[UMLElement[Uml]],
     subElements: Set[UMLElement[Uml]],
     nodes: NodeSeq,
     redefined: MetaPropertyFunctionSet)
    : Set[java.lang.Throwable] \/ SerializationState = {
      val (
        resultingSubElements: Set[UMLElement[Uml]],
        nested: SortedMap[String, scala.xml.Node],
        idrefs: SortedMap[String, scala.xml.Node]) =
        (Tuple3(subElements, SortedMap.empty[String, scala.xml.Node], SortedMap.empty[String, scala.xml.Node]) /: subs) {
          case ((visitedElements, sub_nested, sub_idrefs), subElement) =>
            if (visitedElements.contains(subElement))
              waitGenerateNodeReference(f, subElement) match {
                case -\/(f) =>
                  return -\/(f)
                case \/-(subNode) =>
                  Tuple3(
                    visitedElements + subElement,
                    sub_nested,
                    sub_idrefs + (subElement.xmiID().map(OTI_ID.unwrap).getOrElse("") -> subNode)) // @todo propgate error
              }
            else
              callGenerateNodeElement(f, subElement).run match {
                case -\/(f) =>
                  return -\/(f)
                case \/-(subNode) =>
                  Tuple3(
                    visitedElements + subElement,
                    sub_nested + (subElement.xmiUUID().map(OTI_UUID.unwrap).getOrElse("") -> subNode), // @todo propgate error
                    sub_idrefs)
              }
        }

      val nestedNodes = for {k <- nested.keySet.toSeq} yield nested(k)
      val idrefNodes = for {k <- idrefs.keySet.toSeq} yield idrefs(k)
      val resultNodes = nestedNodes ++ idrefNodes ++ nodes
      \/-((resultingSubElements, resultNodes, redefined))
    }

    def applyGenerateNodeElementsOrSkip
    (f: e.MetaPropertyEvaluator,
     subs: Vector[UMLElement[Uml]],
     subElements: Set[UMLElement[Uml]],
     nodes: NodeSeq,
     redefined: MetaPropertyFunctionSet)
    : Trampoline[Set[java.lang.Throwable] \/ SerializationState]
    = if (subs.isEmpty)
      return_ {
        \/-((subElements, nodes, redefined))
      }
    else {
      val x = subs.head
      val xs = subs.tail
      if (subElements.contains(x))
        return_ {
          \/-((subElements, nodes, redefined))
        }
      else
        append1Pair(x, callGenerateNodeElement(f, x), subElements, nodes, redefined)
          .flatMap {
            case -\/(errors) =>
              return_ {
                -\/(errors)
              }
            case \/-((es, ns, rs)) =>
              applyGenerateNodeElementsOrSkip(f, xs, es, ns, rs)
          }
    }

    /*
     * @see XMI2.5 ptc/14-09-21 9.4.1
     *      Instance of Model Element:
     *      A Property, type is not a PrimitiveType or Enumeration, isComposite = true
     *
     *      XMI Representation:
     *      Choice of:
     *      1. Nested XMIObjectElement
     *      2. Nested XMIReferenceElement
     *      2. Nested XMIReferenceAttribute
     *      Normally, serialized properties with isComposite = true are serialized as nested XMIObjectElements.
     *      In the case where the model is split across more than one file then
     *      a nested XMIReferenceElement would be used.
     *      Exceptionally, even within one file, it may be the case that
     *      a containing object has more than one serialized class-typed property with isComposite = true that
     *      contain the same object or include it among their collection of objects.
     *      In such an exceptional case, because of MOF constraints,
     *      only one of those properties can have an opposite with a non-empty slot.
     *      Objects of the property with the non-empty opposite slot are serialized as nested XMIObjectElements,
     *      and the other references to the same object are serialized either
     *      as XMIReferenceAttributes or nested XMIReferenceElements.
     *
     * @see XMI2.5 ptc/14-09-21 9.4.2
     *      No special serialization rules need to be defined for subsetted Properties.
     *      Following EMOF rule 1, when one of the subsetted or subsetting Properties is derived,
     *      it is not serialized by default. Properties that are not derived are serialized.
     */
    @tailrec def trampolineSubNode
    (nodes: Trampoline[Set[java.lang.Throwable] \/ SerializationState],
     f: e.MetaPropertyEvaluator)
    : Trampoline[Set[java.lang.Throwable] \/ SerializationState]
    = {

      nodes.resume match {
        //        case -\/( s ) =>
        //          suspend { trampolineSubNode( s(), f ) }
        case -\/(s) =>
          trampolineSubNode(s(), f)

        case \/-(r) =>
          r match {
            case -\/(t) =>
              return_ {
                -\/(t)
              }

            case \/-((subElements, ns, redefined)) =>
              f match {
                case rf: e.MetaReferenceEvaluator =>
                  rf
                    .evaluate(e) match {
                    case -\/(t) =>
                      return_ {
                        -\/(t)
                      }
                    case \/-(None) =>
                      return_ {
                        \/-((subElements, ns, redefined))
                      }
                    case \/-(Some(sub)) =>
                      if (subElements.contains(sub))

                      /*
                       * The element is already serialized by a composite meta property.
                       */
                        return_ {
                          \/-((subElements, ns, f.redefinedMetaProperties ++ redefined))
                        }
                      else

                      /*
                       * The element has not yet been serialized; this is the 1st composite meta property to do so.
                       */
                        suspend {
                          prependNestedElement(
                            sub,
                            callGenerateNodeElement(f, sub),
                            subElements,
                            ns,
                            f.redefinedMetaProperties ++ redefined)
                        }
                  }
                case cf: e.MetaCollectionEvaluator =>
                  cf
                    .evaluate(e) match {
                    case -\/(t) =>
                      return_ {
                        -\/(t)
                      }
                    case \/-(subs) =>
                      if (subs.isEmpty)
                        return_ {
                          \/-((subElements, ns, redefined))
                        }
                      else
                        return_ {
                          prependNestedElementsOrIdReferences(f, subs, subElements, ns, redefined)
                        }

                  }
              }
          }
      }
    }

    val refEvaluators
    : Seq[e.MetaPropertyEvaluator] 
    = {
      val allRefs: Seq[e.MetaPropertyEvaluator] = e.referenceMetaProperties
      val redefined
      : Set[e.MetaPropertyEvaluator] 
      = allRefs.to[Set].flatMap { r: e.MetaPropertyEvaluator =>
        r
        .redefinedMetaProperties
        .map(_.asInstanceOf[e.MetaPropertyEvaluator])
      }
      allRefs.filterNot(redefined.contains)
    }
    
    val subEvaluators
    : Seq[e.MetaPropertyEvaluator] 
    = {
      val allComps: Seq[e.MetaPropertyEvaluator] = e.compositeMetaProperties
      val redefined
      : Set[e.MetaPropertyEvaluator]
      = allComps.to[Set].flatMap { r: e.MetaPropertyEvaluator =>
        r.redefinedMetaProperties.map(_.asInstanceOf[e.MetaPropertyEvaluator])
      }
      allComps.filterNot(redefined.contains)
    }
    val duplicates = refEvaluators.toSet.intersect(subEvaluators.toSet)
    require(duplicates.isEmpty, s"${e.xmiType} ${duplicates.size}: $duplicates")

    val mofAttributes0: Set[java.lang.Throwable] \/ scala.xml.MetaData = \/-(scala.xml.Null)
    (mofAttributes0 /: e.mofXMI_metaAtttributes.reverse) (foldAttribute) match {
      case -\/(t) =>
        return_ {
          -\/(t)
        }

      case \/-(mofAttributesN) =>
        suspend {
          val xRefA0: Set[java.lang.Throwable] \/ NodeSeq = \/-(NodeSeq.Empty)
          val xRefAs = (xRefA0 /: e.metaAttributes) (foldAttributeNode)

          val xRefR0: Set[java.lang.Throwable] \/ NodeSeq = \/-(NodeSeq.Empty)
          val xRefRs = (xRefR0 /: refEvaluators) (foldReference)

          // @see http://solitaire.omg.org/secure/EditComment!default.jspa?id=37483&commentId=12422
          // Per Canonical XMI B5.2 Property Elements
          // Issue 17261: clarify the ordering
          // Properties of an element are ordered by the class in which they are defined.
          // Properties defined by a superclass appear before those of its subclasses.
          // Where a class inherits from more than one direct superclass, properties
          // from the class with the alphabetically earlier class name appear
          // before those of an alphabetically later class name.

          // This means traverse the subEvaluators in reverse order (to ensure that the most-specific
          // composite meta-property is the 1st serialization of an object as a nested element)
          // However, trampolineSubNode prepends additions so that the result is in the correct order
          // (I.e., sub-nodes for superclass properties are before sub-nodes for specialized class properties)

          val xSub0: Trampoline[Set[java.lang.Throwable] \/ SerializationState] =
            return_(\/-((Set(), Seq(), Set())))
          val xSubs = (xSub0 /: subEvaluators.reverse) (trampolineSubNode)

          wrapNodes(xRefAs, xSubs, xRefRs, prefix, label, mofAttributesN, xmiScopes)
        }
    }
  }
}