
package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._

import code.control.{OpenIDLogin, TwitterLogin, Logout}

class Boot extends Loggable {
    def boot {
        logger.info("Booting...");
        
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

        //Show the spinny image when an Ajax call starts
        ajaxStart = Full(() => jsArtifacts.show("ajax-loader").cmd)
        ajaxEnd   = Full(() => jsArtifacts.hide("ajax-loader").cmd)

        // Force the request to be UTF-8
        early.append(_.setCharacterEncoding("UTF-8"))
        
        // Handlers for requests
        dispatch.append(Logout.dispatchPF)
        dispatch.append(OpenIDLogin.dispatchPF)
        dispatch.append(TwitterLogin.dispatchPF)
        
        DBSetup()
    }
}

