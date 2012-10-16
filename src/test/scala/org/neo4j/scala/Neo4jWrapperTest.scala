/*
 *                                The MIT License

Copyright (c) 2009, Jawher Moussa and Martin Kleppmann
Modified work Copyright 2012 Alexander Kuprin  


Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

 */

/*
 * Originaly published by Jawher Moussa at https://github.com/akup/neo4j-scala
 * Modified by Alexander Kuprin and published at 
 */

package org.neo4j.scala

import org.specs._
import org.specs.runner._
import org.neo4j.graphdb._
import org.neo4j.kernel._
import scala.collection.JavaConversions._
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.factory.GraphDatabaseSetting


class Neo4jWrapperSpecTest extends JUnit4(Neo4jWrapperSpec)

object Neo4jWrapperSpec extends Specification("Neo4j Wrapper Specification") with Neo4jWrapper {
  "NeoWrapper" should {
    shareVariables()
    implicit val neo = new GraphDatabaseFactory()
            .newEmbeddedDatabaseBuilder( "tmp/temp-neo-test" )
            .setConfig( Map("allow_store_upgrade" -> "true") )
            .newGraphDatabase()
            /*new EmbeddedGraphDatabase("tmp/temp-neo-test"/*,
              Map("allow_store_upgrade" -> "true")*/)*/
    
    Runtime.getRuntime.addShutdownHook(new Thread() {
        override def run() {
          neo.shutdown
        }
      })

    "create a new relationship in --> relType --> notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val end = neo.createNode
        val relType = DynamicRelationshipType.withName("foo")
        start --> relType --> end
        start.getSingleRelationship(relType, Direction.OUTGOING).
          getOtherNode(start) must beEqualTo(end)
      }
    }

    "create a new relationship in --> \"relName\" --> notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val end = neo.createNode
        start --> "foo" --> end
        start.getSingleRelationship(DynamicRelationshipType.withName("foo"), Direction.OUTGOING).
          getOtherNode(start) must beEqualTo(end)
      }
    }

    "create a new relationship in <-- relType <-- notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val end = neo.createNode
        val relType = DynamicRelationshipType.withName("foo")
        end <-- relType <-- start
        start.getSingleRelationship(relType, Direction.OUTGOING).
          getOtherNode(start) must beEqualTo(end)
      }
    }

    "create a new relationship in <-- \"relName\" <-- notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val end = neo.createNode
        end <-- "foo" <-- start
        start.getSingleRelationship(DynamicRelationshipType.withName("foo"), Direction.OUTGOING).
          getOtherNode(start) must beEqualTo(end)
      }
    }

    "allow relationships of the same direction to be chained" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val middle = neo.createNode
        val end = neo.createNode
        start --> "foo" --> middle --> "bar" --> end
        start.getSingleRelationship(DynamicRelationshipType.withName("foo"), Direction.OUTGOING).
          getOtherNode(start) must beEqualTo(middle)
        middle.getSingleRelationship(DynamicRelationshipType.withName("bar"), Direction.OUTGOING).
          getOtherNode(middle) must beEqualTo(end)
      }
    }

    "allow relationships of different directions to be chained" in {
      execInNeo4j { neo =>
        val left = neo.createNode
        val middle = neo.createNode
        val right = neo.createNode
        left --> "foo" --> middle <-- "bar" <-- right
        left.getSingleRelationship(DynamicRelationshipType.withName("foo"), Direction.OUTGOING).
          getOtherNode(left) must beEqualTo(middle)
        right.getSingleRelationship(DynamicRelationshipType.withName("bar"), Direction.OUTGOING).
          getOtherNode(right) must beEqualTo(middle)
      }
    }

    "ignore a relationshipBuilder with no end node" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        start --> "foo"
        start.getRelationships.iterator.hasNext must beEqualTo(false)
      }
    }

    "read a property in a node in node('property') notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        start.setProperty("foo", "bar")
        start("foo") must beEqualTo(Some("bar"))
        start("bar") must beEqualTo(None)
      }
    }

    "create a property in a node in node('property')=value notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        start("foo") = "bar"
        start.getProperty("foo") must beEqualTo("bar")
      }
    }

    "read a property in a relationship in rel('property') notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val end = neo.createNode
        val rel = start.createRelationshipTo(end, DynamicRelationshipType.withName("foo"))
        rel.setProperty("foo", "bar")
        rel("foo") must beEqualTo(Some("bar"))
        rel("bar") must beEqualTo(None)
      }
    }

    "create a property in a relationship in rel('property')=value notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val end = neo.createNode
        val rel = start.createRelationshipTo(end, DynamicRelationshipType.withName("foo"))
        rel("foo") = "bar"
        rel.getProperty("foo") must beEqualTo("bar")
      }
    }

    "allow writing stop evaluators in a functional style" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val end = neo.createNode
        val rel = start.createRelationshipTo(end, DynamicRelationshipType.withName("foo"))
        val traverser = start.traverse(Traverser.Order.BREADTH_FIRST, (tp : TraversalPosition) => false, ReturnableEvaluator.ALL_BUT_START_NODE, DynamicRelationshipType.withName("foo"), Direction.OUTGOING)
        traverser.iterator.hasNext must beEqualTo(true)
        traverser.iterator.next must beEqualTo(end)
      }
    }

    "allow writing returnable evaluators in a functional style" in {
      execInNeo4j { neo =>
        val start = neo.createNode
        val end = neo.createNode
        val rel = start.createRelationshipTo(end, DynamicRelationshipType.withName("foo"))
        val traverser = start.traverse(Traverser.Order.BREADTH_FIRST, StopEvaluator.END_OF_GRAPH, (tp : TraversalPosition) => tp.notStartNode(), DynamicRelationshipType.withName("foo"), Direction.OUTGOING)
        traverser.iterator.hasNext must beEqualTo(true)
        traverser.iterator.next must beEqualTo(end)
      }
    }
  }
}
