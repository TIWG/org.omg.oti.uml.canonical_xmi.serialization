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

import java.lang.Integer
import java.net.URL

import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi._

import scala.{None, Option, Some, StringContext, Unit}
import scala.Predef.{String, require}
import scala.collection.immutable._
import scala.Predef.{Map => _, Set => _, _}
import scala.language.postfixOps
import scala.util.control.Exception._
import scalaz._
import Scalaz._

/**
 * @tparam Uml
 */
trait DocumentIDGenerator[Uml <: UML] extends IDGenerator[Uml] {

  implicit val documentSet: DocumentSet[Uml]

  override implicit val umlOps = documentSet.ops

  import umlOps._

  implicit val documentOps: DocumentOps[Uml] = documentSet.documentOps

  override implicit val otiCharacteristicsProvider
  : OTICharacteristicsProvider[Uml]
  = documentOps.otiCharacteristicsProvider

  protected val element2id: Element2IDHashMap

  protected val elementRules: List[Element2IDRule]

  protected val containmentRules: List[ContainedElement2IDRule]

  protected val element2uuid: Element2UUIDHashMap

  def element2document
  (e: UMLElement[Uml])
  : Option[Document[Uml]]
  = documentSet.lookupDocumentByExtent(e)

  override def element2mappedDocument
  (e: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ Option[Document[Uml]]
  = element2document(e).right

  override def getElement2IDMap
  : Map[UMLElement[Uml], (Set[java.lang.Throwable] \/ (String @@ OTI_ID))]
  = element2id.toMap

  override def getElement2UUIDMap
  : Map[UMLElement[Uml], (Set[java.lang.Throwable] \/ (String @@ OTI_UUID))]
  = element2uuid.toMap

  override def lookupElementXMI_ID
  (e: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ Option[String @@ OTI_ID]
  = element2id
    .get(e)
    .fold[Set[java.lang.Throwable] \/ Option[String @@ OTI_ID]](
      Option.empty[String @@ OTI_ID].right
    ){ id =>
      id.map(_.some)
    }

  override def lookupElementXMI_UUID
  (e: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ Option[String @@ OTI_UUID]
  = element2uuid
      .get(e)
      .fold[Set[java.lang.Throwable] \/ Option[String @@ OTI_UUID]](
      Option.empty[String @@ OTI_UUID].right
    ){ uuid =>
      uuid.map(_.some)
    }

  /**
   * Computes the xmi:ID for each element in the domain of the element2document map of the ResolvedDocumentSet
   */
  def computePackageExtentXMI_ID(pkg: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Unit
  = {
    val extent = pkg
      .allOwnedElements
      .+(pkg)
      .filter(element2document(_).isDefined)

    val e0: Set[java.lang.Throwable] \/ Unit = ().right
    val eN: Set[java.lang.Throwable] \/ Unit = (e0 /: extent) {
      (ei, e) =>
        ei +++ getXMI_ID(e).map( _ => ())
    }

    eN
  }

  /**
    * Computes the xmi:UUID for each element in the domain of the element2document map of the ResolvedDocumentSet
    */
  def computePackageExtentXMI_UUID(pkg: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Unit
  = {
    val extent = pkg
      .allOwnedElements
      .+(pkg)
      .filter(element2document(_).isDefined)

    val e0: Set[java.lang.Throwable] \/ Unit = ().right
    val eN: Set[java.lang.Throwable] \/ Unit = (e0 /: extent) {
      (ei, e) =>
        ei +++ getXMI_UUID(e).map( _ => ())
    }

    eN
  }

  protected def getXMI_IDREF_or_HREF_fragment
  (from: UMLElement[Uml],
   to: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ String
  = getXMI_IDREF_or_HREF_fragment_internal(from, to)
    .orElse(
      getMappedOrReferencedElement(to)
        .flatMap { _to =>
          getXMI_IDREF_or_HREF_fragment(from, _to)
        })

  protected def getXMI_IDREF_or_HREF_fragment_internal
  (from: UMLElement[Uml],
   to: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ String
  = element2document(from)
    .fold[Set[java.lang.Throwable] \/ String] {
      Set(
        UMLError
        .illegalElementError[Uml, UMLElement[Uml]]("Unknown document for element reference from", Iterable(from)))
      .left
    }{ d1 =>
      element2document(to)
      .fold[Set[java.lang.Throwable] \/ String] {
        Set(
          UMLError
          .illegalElementError[Uml, UMLElement[Uml]]("Unknown document for element reference to", Iterable(to)))
        .left
      }{
        case db2: BuiltInImmutableDocument[Uml] =>
          require(d1 != db2)
          val builtIn_d2_id = TOOL_SPECIFIC_ID.unwrap(to.toolSpecific_id)

          // Based on the built-in 'to' element ID, construct the built-in URI for the 'to' element.
          val bid = for {

            builtInURI <- documentOps.getExternalDocumentURL(db2.documentURL)

            builtInURITo =
              builtInURI
              .resolve("#" + builtIn_d2_id)
              .toString

            // use the builtInURIMapper to convert the built-in URI of the 'to' element into an OMG URI
            mappedURITo <- documentSet.builtInURIMapper.resolve(builtInURITo).map(_.getOrElse(builtInURITo))
            fragmentIndex = mappedURITo.lastIndexOf('#')
            _ = require(fragmentIndex > 0)

            // It's not needed to add the prefix since it's already included in the computed ID
            fragment = IDGenerator.xmlSafeID(mappedURITo.substring(fragmentIndex + 1))
          } yield IDGenerator.xmlSafeID(fragment)

          bid

        case db2: BuiltInMutableDocument[Uml] =>
          require(d1 != db2)
          val builtIn_d2_id = TOOL_SPECIFIC_ID.unwrap(to.toolSpecific_id)

          // Based on the built-in 'to' element ID, construct the built-in URI for the 'to' element.
          val bid = for {

            builtInURI <- documentOps.getExternalDocumentURL(db2.documentURL)

            builtInURITo =
            builtInURI
              .resolve("#" + builtIn_d2_id)
              .toString

            // use the builtInURIMapper to convert the built-in URI of the 'to' element into an OMG URI
            mappedURITo <- documentSet.builtInURIMapper.resolve(builtInURITo).map(_.getOrElse(builtInURITo))
            fragmentIndex = mappedURITo.lastIndexOf('#')
            _ = require(fragmentIndex > 0)

            // It's not needed to add the prefix since it's already included in the computed ID
            fragment = IDGenerator.xmlSafeID(mappedURITo.substring(fragmentIndex + 1))
          } yield IDGenerator.xmlSafeID(fragment)

          bid

        case _: Document[Uml] with SerializableDocument =>
          getMappedOrReferencedElement(to)
          .flatMap( _to => getXMI_ID(_to).map(OTI_ID.unwrap))

        case d =>
          Set(
            documentIDGeneratorException(
              this,
              Iterable(from, to),
              s"getXMI_IDREF_or_HREF_fragment_internal: error: Unknown document $d for element reference to"))
          .left
      }
    }

  /**
   * The xmi:ID of an element depends on what kind of document it is contained in.
   * - BuiltInDocument: this is deferred to builtInID, which is implementation-specific.
   * - SerializableDocument: this is the OTI implementation of Canonical XMI ID
   * unless it is overriden by an application of the OTI::Identity stereotype
   */
  def getXMI_ID(self: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ (String @@ OTI_ID)
  = element2id.getOrElseUpdate(
    self, {
      element2document( self )
        .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)] {
          Set(
            documentIDGeneratorException(
              this,
              Iterable(self),
              "getXMI_ID error: Unknown document for element reference"))
          .left
        }{
          case d: Document[Uml] with BuiltInDocument =>
            OTI_ID.apply(TOOL_SPECIFIC_ID.unwrap(self.toolSpecific_id)).right

          case d: Document[Uml] with SerializableDocument =>
            self
            .oti_xmiID
            .flatMap(oid =>
              oid.fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)]{
                computeSingleElementXMI_ID(self)
              }{ id =>
                id.right
              })

          case d =>
            Set(
              documentIDGeneratorException(
                this,
                Iterable(self),
                s"getXMI_ID error: Unrecognized document $d for element"))
            .left
        
      }
    })

  def computeSingleElementXMI_ID(self: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ (String @@ OTI_ID)
  = {
    val r =
      elementRules
      .toStream
      .dropWhile { r: Element2IDRule =>
        !r.isDefinedAt(self)
      }

    if (r.nonEmpty)
      r.head(self)
    else
      self
      .owner
      .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)] {
        Set(
          documentIDGeneratorException(
            this,
            Iterable(self),
            "computeID error: Element without an owner is not supported(1)"))
        .left
      }{ owner =>
        self
        .getContainingMetaPropertyEvaluator()(this)
        .flatMap(ocf =>
          ocf.fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)]{
            -\/(Set[java.lang.Throwable](documentIDGeneratorException[Uml](
              this,
              Iterable(self),
              "computeID error: Element without an owner is not supported(2)")))
          }{ cf =>
            getXMI_ID(owner)
            .flatMap{ ownerID =>
                val c = containmentRules.toStream.dropWhile((c: ContainedElement2IDRule) =>
                  !c.isDefinedAt((owner, ownerID, cf, self)))
                if (c.nonEmpty)
                  c.head((owner, ownerID, cf, self))
                else
                  -\/(Set[java.lang.Throwable](documentIDGeneratorException(
                    this,
                    Iterable(self),
                    "computeID error: Unsupported")))
            }
          })
      }
  }

  /**
    * The xmi:UUID of an element depends on what kind of document it is contained in.
    * - BuiltInDocument: this is deferred to builtInUUID, which is implementation-specific.
    * - SerializableDocument: this is the OTI implementation of Canonical XMI UUID
    * unless it is overriden by an application of the OTI::Identity stereotype
    */
  def getXMI_UUID(self: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ (String @@ OTI_UUID)
  = element2uuid.getOrElseUpdate(
      self, {
        element2document( self )
          .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
          Set(
            documentUUIDGeneratorException(
              this,
              Iterable(self),
              "getXMI_UUID error: Unknown document for element reference"))
            .left
        }{
          case d: Document[Uml] with BuiltInDocument =>
            self
              .toolSpecific_uuid
              .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
              Set(
                documentUUIDGeneratorException(
                  this,
                  Iterable(self),
                  "getXMI_UUID error: Element from a BuiltInDocument without xmi:uuid"))
                .left
            }{ uuid =>
              OTI_UUID.apply(TOOL_SPECIFIC_UUID.unwrap(uuid)).right
            }

          case d: Document[Uml] with SerializableDocument =>
            self
              .oti_xmiUUID
              .flatMap(ouuid =>
                ouuid.fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)]{
                  computeSingleElementXMI_UUID(self)
                }{ uuid =>
                  uuid.right
                })

          case d =>
            Set(
              documentUUIDGeneratorException(
                this,
                Iterable(self),
                s"getXMI_UUID error: Unrecognized document $d for element"))
              .left

        }
      })

  def computeSingleElementXMI_UUID(self: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ (String @@ OTI_UUID)
  = {
    element2mappedDocument(self)
      .flatMap {
        _.fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)](
          Set(
            UMLError
              .illegalElementError[Uml, UMLElement[Uml]](
              s"Cannot generate the OTI uuid for $self because it does not belong to a document",
              Iterable(self)))
            .left
        ) { d =>
          self
            .xmiID()(this)
            .flatMap { id =>
              OTI_UUID(d.info.uuidPrefix + OTI_ID.unwrap(id))
                .right
            }
        }
      }
  }

  val rule0: Element2IDRule = {
    case root: UMLPackage[Uml]
      if (documentSet.lookupDocumentByScope(root).isDefined
          // Adding condition for stopping the ID computation algorithm when a specification root is reached
          || root.hasSpecificationRootCharacteristics) =>
      root
      .name
      .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)] {
        Set(
          documentIDGeneratorException(
            this,
            Iterable(root),
            "rule0 error: Document package scope must be explicitly named"))
        .left
      }{
        n =>
          OTI_ID(IDGenerator.xmlSafeID(n))
          .right
      }
  }

  /**
   * Rule #1 (InstanceValue)
   */
  val crule1: ContainedElement2IDRule = {
    case (owner, ownerID, cf, iv: UMLInstanceValue[Uml]) =>
      iv
      .instance
      .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)] {
        Set(
          documentIDGeneratorException(
            this,
            Iterable(owner, iv),
            "crule1 error: InstanceValue without InstanceSpecification is not supported"))
        .left
      }{ is =>
        is
        .name
        .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)] {
          Set(
            documentIDGeneratorException(
              this,
              Iterable(owner, iv, is),
              "crule1 error: InstanceValue must refer to a named InstanceSpecification"))
          .left
        }{ nInstance =>
          iv.getContainingMetaPropertyEvaluator()(this)
          .flatMap(ocf =>
            ocf.fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)]{
            -\/(Set[java.lang.Throwable](documentIDGeneratorException[Uml](
              this,
              Iterable(owner, iv, is),
              "crule1 error: Element without an owner is not supported(3)")))
          }{ cf =>
            \/-(OTI_ID(OTI_ID.unwrap(ownerID) + "_" + IDGenerator.xmlSafeID(cf.propertyName + "." + nInstance)))
          })
        }
      }
  }

  /**
   * Rule #1 (NamedElement)
   * case (a): Feature or ValueSpecification
   */
  val crule1a: ContainedElement2IDRule = {
    case (owner, ownerID, cf, fv@(_: UMLFeature[Uml] | _: UMLValueSpecification[Uml])) =>
      val fvn = fv.asInstanceOf[UMLNamedElement[Uml]]
      val shortID: Set[java.lang.Throwable] \/ String = owner match {
        case s: UMLSlot[Uml] =>
          s
          .definingFeature
          .fold[Set[java.lang.Throwable] \/ String] {
            Set(
              documentIDGeneratorException(
                this,
                Iterable(owner, fv),
                "crule1a error: Slot must have a defining StructuralFeature"))
            .left
          }{ sf =>
            val slotValues = s.value.toList
            if (sf.upper > 1)
              ("_" + slotValues.indexOf(fvn) + "_" + fvn.name.getOrElse(""))
              .right
            else
              fvn.name
              .getOrElse("")
              .right
          }
        case _ =>
          fvn.name.getOrElse("")
          .right

      }
      val suffix1: Set[java.lang.Throwable] \/ String = shortID.map {
        case "" =>
          ""
        case id =>
          "." + IDGenerator.xmlSafeID(id)
      }
      val suffix2: Set[java.lang.Throwable] \/ String = fv match {
        case bf: UMLBehavioralFeature[Uml] =>
          ( suffix1 /: bf.ownedParameter )( ( s, p ) =>
          s +++
          p._type.fold[Set[java.lang.Throwable] \/ String]{
            Set(
              documentIDGeneratorException(
                this,
                Iterable(owner, fv, p),
                "crule1a error: Parameter must have a type"))
            .left
          }{ t =>
            t
            .name
            .fold[Set[java.lang.Throwable] \/ String] {
              Set(
                documentIDGeneratorException(
                  this,
                  Iterable(owner, fv, p, t),
                  "crule1a error: Type must have a name"))
              .left
            }{ tn =>
              ( "_" + IDGenerator.xmlSafeID( tn ) )
              .right
            }
          } )
        case _ =>
          suffix1
      }
      val suffix3 =
        suffix2
        .flatMap {
          case "" =>
            (owner, owner.owner) match {
              case (s: UMLSlot[Uml], Some(is: UMLInstanceSpecification[Uml])) if cf == Slot_value =>
                s
                .definingFeature
                .fold[Set[java.lang.Throwable] \/ String] {
                  -\/(Set[java.lang.Throwable](documentIDGeneratorException(
                    this,
                    Iterable(is, s),
                    "crule1a error: Slot must have a defining StructuralFeature")))
                }{ sf =>
                  if (sf.upper.intValue() == 1)
                    \/-("")
                  else {
                    val slotValues = s.value.toList
                    val orderedValues =
                      if (cf.isOrdered)
                        slotValues
                      else
                        slotValues
                        .sortBy(
                          _
                            .xmiOrderingKey()(this)
                            .toOption
                            .getOrElse("")) // @todo propagate errors
                    require(orderedValues.contains(fv))
                    \/-(orderedValues.indexOf(fv).toString)
                  }
                }
              case (o1, Some(o2)) =>
                require(
                  !cf.isCollection,
                  s" o1=${o1.toolSpecific_id}, o2=${o2.toolSpecific_id} /"+
                  s" o1=${getXMI_ID(o1).toOption.getOrElse("")}, o2=${getXMI_ID(o2).toOption.getOrElse("")}")
                \/-("")
              case (o1, None) =>
                require(
                  !cf.isCollection,
                  s" o1=${o1.toolSpecific_id} / o1=${getXMI_ID(o1).toOption.getOrElse("")}")
                \/-("")
            }

          case s =>
            \/-(s)

        }

      suffix3.map { s =>
        OTI_ID(ownerID + "_" + IDGenerator.xmlSafeID(cf.propertyName + s))
      }
  }

  /**
   * Rule #1 (NamedElement)
   * case (b): not Feature, not ValueSpecification
   */
  val crule1b: ContainedElement2IDRule = {
    case (owner, ownerID, cf, ne: UMLNamedElement[Uml]) if ne.name.isDefined =>
      OTI_ID(
        OTI_ID.unwrap(ownerID) + "." +
        IDGenerator.xmlSafeID(ne.metaclass_name) + "_" +
        IDGenerator.xmlSafeID(cf.propertyName) + "_" +
        IDGenerator.xmlSafeID(ne.name.getOrElse("")) )
      .right
  }

  /**
   * Rule #2:  any Element on which Rule#1 does not apply and which is owned as an ordered set
   */
  val crule2: ContainedElement2IDRule = {
    case (owner, ownerID, cf, e) if cf.isOrdered && cf.isCollection =>
      e.getElementMetamodelPropertyValue(cf)(this)
      .flatMap{ vs =>
          val values = vs.toList
          require(values.contains(e))
          \/-(OTI_ID(OTI_ID.unwrap(ownerID) + "_" + IDGenerator.xmlSafeID(cf.propertyName) + "." + values.indexOf(e)))
      }
  }

  /**
   * Rule #3: any Element on which Rule#2 does not apply and which is a DirectedRelationship
   *
   * Check if the source & target of a directed relationship are elements of a Built-In Document
   * (e.g., UML Metamodel, StandardProfile, etc...) As of OMG UML 2.5, such source/target elements
   * may have a legacy xmi:ID such as "_0" which is is insufficient to avoid duplication.
   * (For example, a package that imports both UML Metamodel and StandardProfile)
   *
   * To strengthen the unicity and reproducibility of the XMI:ID of the directed relationship,
   * references to elements in built-in documents include the built-in document URI in addition to the
   * xmi:ID of the referenced element.
   */
  val crule3: ContainedElement2IDRule = {
    case (owner, ownerID, cf, dr: UMLDirectedRelationship[Uml]) =>
      (dr.source.toList, dr.target.toList) match {
        case (List(relSource), List(relTarget)) =>
          getXMI_IDREF_or_HREF_fragment(owner, relSource)
            .flatMap { sid =>
              getXMI_IDREF_or_HREF_fragment(owner, relTarget)
                .flatMap { tid =>

                  val sourceID =
                    element2document(relSource) match {
                      case Some(d: Document[Uml] with BuiltInDocument) =>
                        IDGenerator.xmlSafeID(OTI_URI.unwrap(d.info.packageURI) + "." + sid)
                      case _ =>
                        sid
                    }

                  val targetID =
                    element2document(relTarget) match {
                      case Some(d: Document[Uml] with BuiltInDocument) =>
                        IDGenerator.xmlSafeID(OTI_URI.unwrap(d.info.packageURI) + "." + tid)
                      case _ =>
                        tid
                    }

                  val relID =
                    OTI_ID.unwrap(ownerID) +
                      "." + sourceID +
                      "._" + IDGenerator.xmlSafeID(cf.propertyName) +
                      "._" + dr.mofMetaclassName +
                      "." + targetID

                  \/-(OTI_ID(relID))
                }
            }
        case _ =>
          Set(
            documentIDGeneratorException(
              this,
              Iterable(owner, dr),
              "crule3 error: Binary DirectedRelationship must have a target"))
          .left
      }
  }

  /**
   * Rule #4: any Element on which Rule#3 does not apply and which is a uml::Slot
   */
  val crule4: ContainedElement2IDRule = {
    case (owner, ownerID, cf, s: UMLSlot[Uml]) =>
      s
      .definingFeature
      .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)] {
        Set(
          documentIDGeneratorException(
            this,
            Iterable(owner, s),
            "crule4 error: Slot must have a defining StructuralFeature"))
        .left
      }{ sf =>
        sf
        .name
        .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)] {
          Set(
            documentIDGeneratorException(
              this,
              Iterable(owner, s),
              "crule4 error: Slot's defining StructuralFeature must be named"))
          .left
        }{ sfn =>
          OTI_ID(OTI_ID.unwrap(ownerID) + "." + IDGenerator.xmlSafeID(sfn))
          .right
        }
      }
  }

  /**
   * Rule #5: any Element on which Rule#4 does not apply and which is uml::Comment
   */
  val crule5: ContainedElement2IDRule = {
    case (owner, ownerID, cf, c: UMLComment[Uml]) =>
      OTI_ID(OTI_ID.unwrap(ownerID) + "._" + IDGenerator.xmlSafeID(cf.propertyName) + "." + c.getCommentOwnerIndex)
      .right
  }

  /**
   * Rule #6: any Element on which Rule#5 does not apply and which is uml::Image
   */
  val crule6: ContainedElement2IDRule = {
    case (owner, ownerID, cf, i: UMLImage[Uml]) =>
      getImageLocationURL(i) map { locationURL =>
        OTI_ID(OTI_ID.unwrap(ownerID) + "._" + IDGenerator.xmlSafeID(cf.propertyName) + "." + IDGenerator.xmlSafeID(OTI_URL.unwrap(locationURL)))
      }
  }

  def getImageLocationURL(i: UMLImage[Uml]): Set[java.lang.Throwable] \/ (String @@ OTI_URL) =
    i
    .location
    .fold[Set[java.lang.Throwable] \/ (String @@ OTI_URL)] {
      Set(
        documentIDGeneratorException(
          this,
          Iterable(i),
          "getImageLocationURL error: An Image must have a non-null location URL"))
      .left
    }{ loc =>
        nonFatalCatch
        .withApply { cause: java.lang.Throwable =>
          Set(
            documentIDGeneratorException(
              this,
              Iterable(i),
              "getImageLocationURL error",
              cause))
            .left
        }
        .apply({
          val url = new URL(loc) toString;
          OTI_URL(IDGenerator.getValidNCName(url))
          .right
        })
    }

  def checkIDs()
  : Set[java.lang.Throwable] \/ Unit
  = {
    // supersafe 1.1.1 thinks that the inferred type is:
    // scala.collection.mutable.HashMap[
    // Object{type Tag = org.omg.oti.json.common.OTIPrimitiveTypes.OTI_ID; type Self = String},
    // org.omg.oti.uml.read.api.UMLElement[Uml]]
    //val id2Element = scala.collection.mutable.HashMap[String @@ OTI_ID, UMLElement[Uml]]()
    val id2Element = scala.collection.mutable.HashMap[String, UMLElement[Uml]]()
    var duplicates: Integer = 0
    var failed: Integer = 0
    println("\n>>> IDs Checking...")

    val res0: Set[java.lang.Throwable] \/ Unit = \/-(())
    val resN: Set[java.lang.Throwable] \/ Unit = ( res0 /: getElement2IDMap ) {
      case ( resi, (ei, maybeId)) =>
      resi +++
      maybeId
      .fold[Set[java.lang.Throwable] \/ Unit](
        (errors: Set[java.lang.Throwable]) => {
          failed = failed + 1
          println(s"***ID computation failed for ${ei.toWrappedObjectString}")
          for {
            error <- errors
          } println("\tCause: " + error.getMessage)
          println("---------------------------")
          -\/(errors)
        },

        (id: String @@ OTI_ID) => {
          val sid = OTI_ID.unwrap(id)
          id2Element
            .get(sid)
            .fold[Set[java.lang.Throwable] \/ Unit]({
            id2Element.update(sid, ei)
            \/-(())
          }) { e =>
            if (e == ei)
              \/-(())
            else {
              duplicates = duplicates + 1
              val message =
                s"*** Duplicate ID: $id\n" +
                  s"\t-> ${ei.toWrappedObjectString}\n" +
                  s"\t-> ${e.toWrappedObjectString}"
              println("---------------------------")
              println(message)
              println("---------------------------")
              -\/(Set(UMLError.umlAdaptationError(message)))
            }
          }
        }
      )
    }
    println(s"<<<... IDs Checked ($duplicates duplicates, $failed failed)\n")
    resN
  } // end CheckIds

  def listIDs(): Unit = {
    println(s"element2id size=${element2id.size}")
    for ((x, id) <- element2id) {
      val idStr =
        id
        .fold[String](
          (errors: Set[java.lang.Throwable]) =>
          s"*** Fail: ${errors.size} erors",
          (s: String @@ OTI_ID) =>
          OTI_ID.unwrap(s))
      x match {
        case ne: UMLNamedElement[Uml] =>
          val nameStr =
            ne.name
            .fold("(unnamed)") { identity _ }
          println("<" + ne.mofMetaclassName + ">" + nameStr + "\t=> ID: " + idStr)

        case e: UMLElement[Uml] =>
          println("<" + e.mofMetaclassName + ">\t=> ID: " + idStr)
      }
    }
  }

}