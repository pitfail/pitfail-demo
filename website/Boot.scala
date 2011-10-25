
package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._

import code.control

import texttrading.{TwitterFrontend, PitFailBackend}
import code.keys.Twitter


class Boot extends Loggable {
    def boot {
        logger.info("Booting...");

        /* Start the twitter server */
        (new TwitterFrontend(Twitter, new PitFailBackend)).run()

        val lr = LiftRules.realInstance
        import lr.{
            setSiteMap, ajaxStart, ajaxEnd, jsArtifacts, early,
            dispatch
        }

        // where to search snippet
        LiftRules.addToPackages("code")

        val entries = List(
            Menu.i("Home") / "index",
            Menu.i("Test") / "testform"
        )
        setSiteMap(SiteMap(entries:_*))

        early.append(_.setCharacterEncoding("UTF-8"))

        // Handlers for requests
        dispatch.append(control.Logout.dispatchPF)
        dispatch.append(control.OpenIDLogin.dispatchPF)
        dispatch.append(control.TwitterLogin.dispatchPF)

        DBSetup()

        // Runs every 30 minutes
        control.DerivativeChecker.run()
    }
}

