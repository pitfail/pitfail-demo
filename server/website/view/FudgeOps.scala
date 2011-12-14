
// Written by: Owen Healy

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
import stockdata._

// Joda Time
import org.joda.time.DateTime

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
    
    val insertTestData = FormSubmit.rendered("Insert Test Data (AND DESTROY ALL!!!!)") {
        bootstrap.liftweb.insertTestData()
    }
    
    val niceTestData = FormSubmit.rendered("Insert Test Data Nicely") {
        bootstrap.liftweb.niceTestData()
    }
    
    val sendNewsletter = FormSubmit.rendered("Send Newsletter") {
        Newsletter.runNewsletter()
    }
    
    val insertDividends = FormSubmit.rendered("Add Dividends") {
        val now = new DateTime
        for (offset <- 1 to 60) {
            model.Stocks.syntheticDividends ++= List(
                Dividend("MSFT", now plusMinutes offset, Price("0.30"))
            )
        }
    }
    
    <ul>
    {runChecks}
    {clearDatabase}
    {insertTestData}
    {niceTestData}
    {sendNewsletter}
    {insertDividends}
    </ul>
}

//
}

