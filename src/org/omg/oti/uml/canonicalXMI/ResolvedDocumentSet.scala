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

import java.io.{FileOutputStream, OutputStreamWriter, PrintWriter}
import java.lang.IllegalArgumentException

import org.omg.oti.uml._
import org.omg.oti.uml.OTIPrimitiveTypes._
import org.omg.oti.uml.read.UMLStereotypeTagValue
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi._

import scala.{Boolean, Function0, Option, None, Some, StringContext, Tuple2, Tuple3, Unit}
import scala.Predef.{Set => _, Map => _, _}
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.language.{higherKinds, implicitConversions, postfixOps}
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
  * @param element2document Map from `UMLElement` to its containing `Document` node in the graph
  * @param unresolvedElementMapper A partial function for mapping unresolved `UMLElement` to a resolvable `UMLElement`
  * @tparam Uml The type signature for a tool-specific adaptation of the OTI UML API
  */
case class ResolvedDocumentSet[Uml <: UML]
(ds: DocumentSet[Uml],
 g: DocumentSet[Uml]#MutableDocumentSetGraph,
 protected val element2document: Map[UMLElement[Uml], Document[Uml]],
 unresolvedElementMapper: UMLElement[Uml] => Option[UMLElement[Uml]]) {

  implicit val dOps = ds.documentOps

  def isElementMapped2Document(e: UMLElement[Uml]): Boolean =
    element2mappedDocument(e).nonEmpty

  def element2mappedDocument(e: UMLElement[Uml]): Option[Document[Uml]] =
    element2document
      .get(e)
      .orElse {
        unresolvedElementMapper(e)
          .flatMap { em =>
            element2document.get(em)
          }
      }

  def getStereotype_ID_UUID
  (s: UMLStereotype[Uml])
  (implicit idg: IDGenerator[Uml]) 
  : NonEmptyList[java.lang.Throwable] \/ (String @@ OTI_ID, String @@ OTI_UUID) = {
    s
      .xmiID()
      .flatMap { _id =>
        s
          .xmiUUID()
          .flatMap { _uuid =>

            element2mappedDocument(s)
              .fold[NonEmptyList[java.lang.Throwable] \/ (String @@ OTI_ID, String @@ OTI_UUID)](
              -\/(
                NonEmptyList(
                  resolvedDocumentSetException(
                    this,
                    "getStereotype_ID_UUID failed",
                    UMLError
                      .illegalElementError[Uml, UMLStereotype[Uml]](
                      s"There should be a document for stereotype ${s.qualifiedName.get} (ID=${_id})",
                      Iterable(s)))))
            ) {
              case d: BuiltInDocument[Uml] =>
                dOps
                .getExternalDocumentURL(d.documentURL)
                .flatMap { url =>
                  catching(
                    classOf[java.lang.NullPointerException],
                    classOf[java.lang.IllegalArgumentException])
                    .withApply {
                      (cause: java.lang.Throwable) =>
                        -\/(
                          NonEmptyList(
                            resolvedDocumentSetException(
                              this,
                              "getStereotype_ID_UUID failed",
                              UMLError
                                .illegalElementError[Uml, UMLStereotype[Uml]](
                                s"There should be a document for stereotype ${s.qualifiedName.get} (ID=${_id})",
                                Iterable(s)))))
                    }
                    .apply({
                      val builtInURI = url.resolve("#" + _id).toString
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

              case d: SerializableDocument[Uml] =>
                \/-(Tuple2(_id, _uuid))

              case d: Document[Uml] =>
                -\/(
                  NonEmptyList(
                    resolvedDocumentSetException(
                      this,
                      "getStereotype_ID_UUID failed",
                      UMLError
                        .illegalElementError[Uml, UMLStereotype[Uml]](
                        s"Unrecognized document $d for stereotype ${s.qualifiedName.get} (ID=${_id})",
                        Iterable(s)))))
            }
          }
      }
  }

  def lookupDocumentByScope(e: UMLElement[Uml]): Option[Document[Uml]] =
    element2mappedDocument(e)
      .filter(d => d.scope == e)

  def serialize
  ()
  (implicit idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] = {

    val s0: NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] = Set().right
    val sN: NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] = (s0 /: g.nodes) {
      (si, n) => n.value match {
        case _: BuiltInDocument[Uml] =>
          si
        case d: SerializableDocument[Uml] =>
          si +++ serialize(d)
      }
    }

    sN
  }

  def serializePkg
  (pkg: UMLPackage[Uml])
  (implicit idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] =
    ds
      .serializableDocuments
      .find { d =>
        d.scope == pkg
      }
      .fold[NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)]] {
      NonEmptyList(
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
  (tagValueNodes: NonEmptyList[java.lang.Throwable] \/ List[scala.xml.Elem],
   stereotypeTagValue: UMLStereotypeTagValue[Uml])
  : NonEmptyList[java.lang.Throwable] \/ List[scala.xml.Elem] =
    tagValueNodes +++ stereotypeTagValue.serialize(xmiScopes, idg).map(_.to[List])

  protected def serialize
  (d: SerializableDocument[Uml])
  (implicit idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] =
    \/.fromTryCatchNonFatal(new java.net.URI(OTI_URI.unwrap(d.info.packageURI)))
      .fold[NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)]](
    l = (t: java.lang.Throwable) =>
      -\/(
        NonEmptyList[java.lang.Throwable](
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
        val result: NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] =
          \/.fromTryCatchNonFatal(new java.io.File(uri)) match {
            case -\/(t) =>
              -\/(
                NonEmptyList[java.lang.Throwable](
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
  (d: SerializableDocument[Uml],
   d_id: String @@ OTI_ID,
   d_uuid: String @@ OTI_UUID,
   furi: java.io.File)
  (implicit idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] = {

    val dir = furi.getParentFile
    dir.mkdirs()

    val tv0: NonEmptyList[java.lang.Throwable] \/ Set[(UMLElement[Uml], Seq[UMLStereotypeTagValue[Uml]])] =
      Set().right
    val tvN: NonEmptyList[java.lang.Throwable] \/ Set[(UMLElement[Uml], Seq[UMLStereotypeTagValue[Uml]])] =
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
              .flatMap(_.appliedStereotype.profile.filter(element2document.contains(_)))
        }

      val profiles: List[UMLProfile[Uml]] =
        referencedProfiles.toList.sortBy(_.qualifiedName.get)

      serialize(d, d_id, d_uuid, furi, element2stereotypeTagValues.toMap, profiles)
    }
  }

  protected def serialize
  (d: SerializableDocument[Uml],
   d_id: String @@ OTI_ID,
   d_uuid: String @@ OTI_UUID,
   furi: java.io.File,
   element2stereotypeTagValues: Map[UMLElement[Uml], Seq[UMLStereotypeTagValue[Uml]]],
   referencedProfiles: List[UMLProfile[Uml]])
  (implicit idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] = {
    import DocumentSet._
    import scala.xml._

    val s0: NonEmptyList[java.lang.Throwable] \/ NamespaceBinding =
      \/-(null)
    val sN: NonEmptyList[java.lang.Throwable] \/ NamespaceBinding =
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
  (d: SerializableDocument[Uml],
   d_id: String @@ OTI_ID,
   d_uuid: String @@ OTI_UUID,
   furi: java.io.File,
   element2stereotypeTagValues: Map[UMLElement[Uml], Seq[UMLStereotypeTagValue[Uml]]],
   referencedProfiles: List[UMLProfile[Uml]],
   xmiScopes: scala.xml.NamespaceBinding)
  (implicit idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Set[(SerializableDocument[Uml], java.io.File)] = {
    import scala.xml._

    val elementOrdering = scala.collection.mutable.ArrayBuffer[UMLElement[Uml]]()

    val free: Free[Function0, NonEmptyList[java.lang.Throwable] \/ scala.xml.Node] =
      generateNodeElement(
        elementOrdering,
        d, "uml", d.scope.xmiElementLabel,
        d.scope, xmiScopes)

    val result: NonEmptyList[java.lang.Throwable] \/ scala.xml.Node =
      free.go(f => Comonad[Function0].copoint(f))(Applicative[Function0])

    // alternatively:
    // val result = free.run

    result
      .flatMap { top =>
        val mofTagRef: MetaData =
          new PrefixedAttribute(pre = "xmi", key = "idref", value = OTI_ID.unwrap(d_id), Null)
        val mofTagElement: Node =
          Elem(
            prefix = null, label = "element", attributes = mofTagRef,
            scope = xmiScopes, minimizeEmpty = true)
        val mofTag = Elem(
          prefix = "mofext",
          label = "Tag",
          attributes =
            new PrefixedAttribute(
              pre = "xmi", key = "id", value = d_id + "_mofext.Tag",
              new PrefixedAttribute(
                pre = "xmi", key = "uuid", value = d_uuid + "_mofext.Tag",
                new PrefixedAttribute(
                  pre = "xmi", key = "type", value = "mofext:Tag",
                  d.scope match {
                    case ne: UMLNamedElement[Uml] =>
                      ne
                        .name
                        .fold[MetaData](Null) { name =>
                        new UnprefixedAttribute(
                          key = "name", value = "org.omg.xmi.nsPrefix",
                          new UnprefixedAttribute(
                            key = "value", value = name,
                            Null))
                      }
                    case _ =>
                      Null
                  }))),
          scope = xmiScopes,
          minimizeEmpty = true,
          mofTagElement)

        val sTV0: NonEmptyList[java.lang.Throwable] \/ List[Node] = List[Node]().right
        val sTVN: NonEmptyList[java.lang.Throwable] \/ List[Node] = (sTV0 /: elementOrdering.to[List]) { (sTVi, e) =>
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
                  tagValues.map(_.appliedStereotype).toSet filter element2document.contains
                }

              val ordering: List[UMLStereotype[Uml]] =
                appliedStereotypes
                  .toList
                  .sortBy(
                    getStereotype_ID_UUID(_)
                    .getOrElse(Tuple2(OTI_ID(""), OTI_UUID(""))) // @todo propagate errors
                    match { case (id, uuid) => OTI_ID.unwrap(id) + OTI_UUID.unwrap(uuid) }
                  )


              val oTVE0: NonEmptyList[java.lang.Throwable] \/ List[Node] = List[Node]().right
              val oTVEN: NonEmptyList[java.lang.Throwable] \/ List[Node] = (oTVE0 /: ordering) { (oTVEi, s) =>
                getStereotype_ID_UUID(s)
                  .flatMap {
                    case (sID, sUUID) =>
                      val tagValueAttributes: NonEmptyList[java.lang.Throwable] \/ List[Elem] =
                        allTagValuesByStereotype
                          .get(s)
                          .fold[NonEmptyList[java.lang.Throwable] \/ List[Elem]](List[Elem]().right) { vs =>
                          val tagValueAttribute0: NonEmptyList[java.lang.Throwable] \/ List[Elem] = List[Elem]().right
                          val tagValueAttributeN = (tagValueAttribute0 /: vs) (foldTagValues(xmiScopes, idg))
                          tagValueAttributeN
                        }
                      val stAppID = IDGenerator.computeStereotypeApplicationID(eID, sID)
                      val stAppUUID = IDGenerator.computeStereotypeApplicationUUID(eUUID, sUUID)
                      val xmiTagValueAttributes =
                        new PrefixedAttribute(
                          pre = "xmi", key = "id", value = OTI_ID.unwrap(stAppID),
                          new PrefixedAttribute(
                            pre = "xmi", key = "uuid", value = OTI_UUID.unwrap(stAppUUID),
                            new PrefixedAttribute(
                              pre = "xmi", key = "type", value = s.profile.get.name.get + ":" + s.name.get,
                              Null)))

                      tagValueAttributes.map { tVAs =>
                        List(Elem(
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

            val xmi = Elem(
              prefix = "xmi",
              label = "XMI",
              attributes = Null,
              scope = xmiScopes,
              minimizeEmpty = true,
              top :: mofTag :: stereotypeTagValues: _*)

            val filepath = furi.getPath + ".xmi"
            \/.fromTryCatchNonFatal[java.io.File]({
              val xmlFile = new java.io.File(filepath)
              val xmlPrettyPrinter = new PrettyPrinter(width = 300, step = 2)
              val xmlOutput = xmlPrettyPrinter.format(xmi)
              val bw = new PrintWriter(new OutputStreamWriter(new FileOutputStream(xmlFile), "UTF-8"))
              bw.println("<?xml version='1.0' encoding='UTF-8'?>")
              bw.println(xmlOutput)
              bw.close()
              xmlFile
            }) match {
              case -\/(t) =>
                NonEmptyList[java.lang.Throwable](
                  resolvedDocumentSetException(
                    this,
                    s"serialize failed: Cannot save XMI serialization "
                      + s"${d.info.packageURI} to file: $filepath: ${t.getMessage}",
                    t)).left
              case \/-(file) =>
                Set[(SerializableDocument[Uml], java.io.File)]((d, file)).right
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
   t: Trampoline[NonEmptyList[java.lang.Throwable] \/ scala.xml.Node],
   subElements: Set[UMLElement[Uml]],
   nodes: Seq[scala.xml.Node],
   redefinitions: MetaPropertyFunctionSet)
  : Trampoline[NonEmptyList[java.lang.Throwable] \/ SerializationState] = {

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
   t: Trampoline[NonEmptyList[java.lang.Throwable] \/ scala.xml.Node],
   subElements: Set[UMLElement[Uml]],
   nodes: Seq[scala.xml.Node],
   redefinitions: MetaPropertyFunctionSet)
  : Trampoline[NonEmptyList[java.lang.Throwable] \/ SerializationState] = {

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
   t: Trampoline[NonEmptyList[java.lang.Throwable] \/ scala.xml.Node])
  : Trampoline[NonEmptyList[java.lang.Throwable] \/ NodeSeq] = {

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
  (t1: Trampoline[NonEmptyList[java.lang.Throwable] \/ NodeSeq],
   t2: Trampoline[NonEmptyList[java.lang.Throwable] \/ scala.xml.Node])
  : Trampoline[NonEmptyList[java.lang.Throwable] \/ NodeSeq] = {

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
  (xRefAttrs: NonEmptyList[java.lang.Throwable] \/ NodeSeq,
   t: Trampoline[NonEmptyList[java.lang.Throwable] \/ SerializationState],
   xRefRefs: NonEmptyList[java.lang.Throwable] \/ NodeSeq,
   prefix: String,
   label: String,
   xmlAttributesAndLocalReferences: scala.xml.MetaData,
   xmiScopes: scala.xml.NamespaceBinding)
  : Trampoline[NonEmptyList[java.lang.Throwable] \/ scala.xml.Node] = {

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
   d: SerializableDocument[Uml],
   prefix: String,
   label: String,
   e: UMLElement[Uml],
   xmiScopes: scala.xml.NamespaceBinding)
  (implicit idg: IDGenerator[Uml])
  : Trampoline[NonEmptyList[java.lang.Throwable] \/ scala.xml.Node] = {

    elementOrdering += e

    import scala.xml._

    //    assert(
    //      Thread.currentThread().getStackTrace.count( _.getMethodName == "generateNodeElement" ) == 1,
    //      s"Verification that the trampolined function 'wrapNodes' runs recursively " +
    //      s"stack-free for label=${label}, e=${e.xmiID}" )

    def foldAttribute
    (next: NonEmptyList[java.lang.Throwable] \/ MetaData, f: e.MetaAttributeFunction)
    : NonEmptyList[java.lang.Throwable] \/ MetaData =
      (next, f.evaluate(e, idg, idg.otiCharacteristicsProvider)) match {
        case (-\/(t), _) =>
          t.left
        case (_, -\/(t)) =>
          t.left
        case (\/-(n), \/-(values)) =>
          (n /: values) {
            case (_n, _value) =>
              f
              .attributePrefix
              .fold[Attribute] {
                new UnprefixedAttribute(key = f.attributeName, value = _value, _n)
              } { aPrefix =>
                new PrefixedAttribute(pre = aPrefix, key = f.attributeName, value = _value, _n)
              }
          }.right
      }

    def foldAttributeNode
    (nodes: NonEmptyList[java.lang.Throwable] \/ NodeSeq, f: e.MetaAttributeFunction)
    : NonEmptyList[java.lang.Throwable] \/ NodeSeq =
      (nodes, f.evaluate(e, idg, idg.otiCharacteristicsProvider)) match {
        case (-\/(t), _) =>
          t.left
        case (_, -\/(t)) =>
          t.left
        case (\/-(ns), \/-(values)) =>
          val valueNodes = for {
            value <- values
          } yield Elem(
            prefix = null, label = f.attributeName,
            attributes = Null, scope = xmiScopes, minimizeEmpty = true, Text(value))
          (ns ++ valueNodes).right
      }

    def foldReference
    (nodes: NonEmptyList[java.lang.Throwable] \/ NodeSeq, f: e.MetaPropertyEvaluator)
    : NonEmptyList[java.lang.Throwable] \/ NodeSeq =
      nodes.flatMap { ns: NodeSeq =>
        f match {
          case rf: e.MetaReferenceEvaluator =>
            rf
            .evaluate(e)
            .flatMap(
              _.fold[NonEmptyList[java.lang.Throwable] \/ NodeSeq](
                ns.right
              ){ eRef =>
                eRef
                .xmiID()
                .flatMap { eRefID =>
                  element2mappedDocument(eRef)
                  .fold[NonEmptyList[java.lang.Throwable] \/ NodeSeq](
                    ns.right
                  ){ dRef =>
                    if (d == dRef) {
                      val idrefAttrib: MetaData =
                        new PrefixedAttribute(pre = "xmi", key = "idref", value = OTI_ID.unwrap(eRefID), Null)
                      val idrefNode: Node =
                        Elem(
                          prefix = null, label = f.propertyName,
                          attributes = idrefAttrib, scope = xmiScopes, minimizeEmpty = true)
                      (ns :+ idrefNode).right
                    } else {
                      val href = dRef.documentURL + "#" + eRefID
                      val externalHRef: NonEmptyList[java.lang.Throwable] \/ String =
                        dRef match {
                          case _: SerializableDocument[Uml] =>
                            href.right
                          case _: BuiltInDocument[Uml] =>
                            ds.builtInURIMapper.resolve(href).map(_.getOrElse(href))
                        }

                      externalHRef
                      .map { exhref =>
                        val hrefAttrib: MetaData =
                          new UnprefixedAttribute(key = "href", value = exhref, Null)
                        val hrefNode: Node =
                          Elem(
                            prefix = null, label = f.propertyName, attributes = hrefAttrib,
                            scope = xmiScopes, minimizeEmpty = true)
                        (ns :+ hrefNode)
                      }
                    }
                  }
                }
              })

          case cf: e.MetaCollectionEvaluator =>
            cf
              .evaluate(e)
              .flatMap { eRefs: List[UMLElement[Uml]] =>
                if (eRefs.isEmpty)
                  ns.right
                else {
                  val ordered_eRefs: List[UMLElement[Uml]] =
                    if (cf.isOrdered)
                      eRefs
                    else
                      eRefs.sortBy(_.xmiOrderingKey.getOrElse("")) // @todo propagate errors
                  val hRef0: NonEmptyList[java.lang.Throwable] \/ NodeSeq = NodeSeq.Empty.right
                  val hRefN: NonEmptyList[java.lang.Throwable] \/ NodeSeq = (hRef0 /: ordered_eRefs ) {
                    (hRefi, eRef) =>
                      eRef.xmiID()
                        .flatMap { eRefID =>
                          element2mappedDocument(eRef)
                            .fold[NonEmptyList[java.lang.Throwable] \/ NodeSeq](
                            NodeSeq.Empty.right
                          ) { dRef =>
                            val dNodes: NonEmptyList[java.lang.Throwable] \/ NodeSeq =
                              if (d == dRef) {
                                val idrefAttrib: MetaData =
                                  new PrefixedAttribute(pre = "xmi", key = "idref", value = OTI_ID.unwrap(eRefID), Null)
                                val idrefNode: Node =
                                  Elem(
                                    prefix = null, label = f.propertyName,
                                    attributes = idrefAttrib, scope = xmiScopes, minimizeEmpty = true)
                                \/-(idrefNode)
                              } else {
                                val href = dRef.documentURL.toString + "#" + eRefID
                                val externalHRef: NonEmptyList[java.lang.Throwable] \/ String =
                                  dRef match {
                                    case _: SerializableDocument[Uml] =>
                                      href.right
                                    case _: BuiltInDocument[Uml] =>
                                      ds.builtInURIMapper.resolve(href).map(_.getOrElse(href))
                                  }

                                externalHRef
                                  .map { exhref =>
                                    val hrefAttrib: MetaData =
                                      new UnprefixedAttribute(key = "href", value = exhref, Null)
                                    val hrefNode: Node =
                                      Elem(
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
    : NonEmptyList[java.lang.Throwable] \/ scala.xml.Node =
      callGenerateNodeElement(f, sub).run

    def callGenerateNodeElement
    (f: e.MetaPropertyEvaluator,
     sub: UMLElement[Uml])
    : Trampoline[NonEmptyList[java.lang.Throwable] \/ scala.xml.Node] =
      generateNodeElement(elementOrdering, d, null, f.propertyName, sub, xmiScopes)

    def waitGenerateNodeReference
    (f: e.MetaPropertyEvaluator,
     sub: UMLElement[Uml])
    : NonEmptyList[java.lang.Throwable] \/ scala.xml.Node = {
      val idRefAttrib: MetaData =
        new PrefixedAttribute(
          pre = "xmi",
          key = "idref",
          value = sub.xmiID().map(OTI_ID.unwrap).getOrElse(""), // @todo propagate errors
          Null)

      val idRefNode: Node = Elem(
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
    : Trampoline[NonEmptyList[java.lang.Throwable] \/ scala.xml.Node] = {
      return_ {
        waitGenerateNodeReference(f, sub)
      }
    }

    def prependNestedElementsOrIdReferences
    (f: e.MetaPropertyEvaluator,
     subs: List[UMLElement[Uml]],
     subElements: Set[UMLElement[Uml]],
     nodes: NodeSeq,
     redefined: MetaPropertyFunctionSet)
    : NonEmptyList[java.lang.Throwable] \/ SerializationState = {
      val (
        resultingSubElements: Set[UMLElement[Uml]],
        nested: SortedMap[String, Node],
        idrefs: SortedMap[String, Node]) =
        (Tuple3(subElements, SortedMap.empty[String, Node], SortedMap.empty[String, Node]) /: subs) {
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
     subs: List[UMLElement[Uml]],
     subElements: Set[UMLElement[Uml]],
     nodes: NodeSeq,
     redefined: MetaPropertyFunctionSet)
    : Trampoline[NonEmptyList[java.lang.Throwable] \/ SerializationState] =
      subs match {
        case Nil =>
          return_ {
            \/-((subElements, nodes, redefined))
          }
        case x :: xs =>
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
    (nodes: Trampoline[NonEmptyList[java.lang.Throwable] \/ SerializationState],
     f: e.MetaPropertyEvaluator)
    : Trampoline[NonEmptyList[java.lang.Throwable] \/ SerializationState] = {

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

    val refEvaluators: Seq[e.MetaPropertyEvaluator] = e.referenceMetaProperties
    val subEvaluators: Seq[e.MetaPropertyEvaluator] = e.compositeMetaProperties
    val duplicates = refEvaluators.toSet.intersect(subEvaluators.toSet)
    require(duplicates.isEmpty, s"${e.xmiType} ${duplicates.size}: $duplicates")

    val mofAttributes0: NonEmptyList[java.lang.Throwable] \/ MetaData = \/-(Null)
    (mofAttributes0 /: e.mofXMI_metaAtttributes.reverse) (foldAttribute) match {
      case -\/(t) =>
        return_ {
          -\/(t)
        }

      case \/-(mofAttributesN) =>
        suspend {
          val xRefA0: NonEmptyList[java.lang.Throwable] \/ NodeSeq = \/-(NodeSeq.Empty)
          val xRefAs = (xRefA0 /: e.metaAttributes) (foldAttributeNode)

          val xRefR0: NonEmptyList[java.lang.Throwable] \/ NodeSeq = \/-(NodeSeq.Empty)
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

          val xSub0: Trampoline[NonEmptyList[java.lang.Throwable] \/ SerializationState] =
            return_(\/-((Set(), Seq(), Set())))
          val xSubs = (xSub0 /: subEvaluators.reverse) (trampolineSubNode)

          wrapNodes(xRefAs, xSubs, xRefRs, prefix, label, mofAttributesN, xmiScopes)
        }
    }
  }
}
