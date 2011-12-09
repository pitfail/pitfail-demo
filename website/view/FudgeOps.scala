
package code
package snippet

import net.liftweb.common.Loggable
import scala.xml._

import intform._

import net.liftweb.http._
import js._
import JsCmds._
import JE._

import model._
import model.schema._

import control._

class FudgeOps extends Loggable {
//

def render = {
    val runChecks = FormSubmit.rendered("Run Checks") {
        Checker.runChecks()
    }
    
    val clearDatabase = FormSubmit.rendered("Clear Database") {
        readDB { create_! }
    }
    
    val insertTestData = FormSubmit.rendered("Insert Test Data") {
        bootstrap.liftweb.insertTestData()
    }
    
    val sendNewsletter = FormSubmit.rendered("Send Newsletter") {
        Newsletter.runNewsletter()
    }
    
    <ul>
    {runChecks}
    {clearDatabase}
    {insertTestData}
    {sendNewsletter}
    </ul>
}

//
}

