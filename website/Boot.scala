
package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._

import code.control

class Boot extends Loggable {

    def boot {
        org.apache.log4j.PropertyConfigurator.configure("log4j.properties")
        
        System.err.println("\033[42m--------------------------------------------------------\033[0m")
        System.err.println
        System.err.println("\033[42m--------------------------------------------------------\033[0m")
        System.err.println

        /* Start the twitter server */
        // I'm sorry I take this out for testing
        // texttrading.twit.run()

        val lr = LiftRules.realInstance
        import lr.{
            setSiteMap, ajaxStart, ajaxEnd, jsArtifacts, early,
            dispatch
        }
        
        // Look at this example:
        // https://gist.github.com/166669
        LiftRules.passNotFoundToChain = true
        LiftRules.liftRequest.append {
            case req
                if (req.path.partPath match {
                    case "servlet" :: _ => true
                    case _              => false
                })
                => false
        }
        
        // where to search snippet
        LiftRules.addToPackages("code")

        val entries = List(
            Menu.i("Home") / "index",
            Menu.i("Test") / "testform",
            Menu.i("Schema") / "schema",
            Menu.i("1") / "auction",
            Menu.i("2") / "user",
            Menu.i("3") / "event"
        )
        setSiteMap(SiteMap(entries:_*))

        early.append(_.setCharacterEncoding("UTF-8"))

        // Handlers for requests
        dispatch.append(control.Logout.dispatchPF)
        dispatch.append(control.OpenIDLogin.dispatchPF)
        dispatch.append(control.TwitterLogin.dispatchPF)

        DBSetup()

        // Runs every 30 minutes
        control.Checker.run()
        
        // Test data
        insertTestData()
    }
}

