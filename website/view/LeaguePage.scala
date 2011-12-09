package code
package snippet

/* Lift stuff */
import net.liftweb.{common,http,util}
import http.S
import util._
import common._
//import js._
//import JsCmds._
//import JE._

/* Form code (custom) */
import intform._

/* Model (pitfail) */
import model._
import model.schema._

import scala.xml._

import stockdata.{HttpQueryService => HQS}


class LeagueManager extends Page with Loggable
{
    def render = {
        <p>LOL Management</p>
    }
}

class  LeagueCreator extends Page with Loggable
{
    import LeagueCreator.NewLeague
    def render = form.render

    lazy val form: Form[NewLeague] = Form(
        NewLeague,
        (
            name_f: Field[String],
            cash_f: Field[Dollars]
        ),
        <div class="boxy new-league">
            <h3>Make a league:</h3>
            <p>League Name: {name_f.main & <input id="name" class="blank"/>}</p>
            <p>Starting Cash:{cash_f.main & <input id="cash" class="blank"/>}</p>

            {
            /*
            <div>{
                    (for (_ <- range(1, S.param("invites").getOrElse(1))) {
                            <p>i</p>
                    } yield) /: (++)
            }</div>
            */
            }

            <p>{submit.main & <input/>}</p>
            <p>{submit.errors}</p>
        </div>
    )

    private def createLeague(name: String, cash: Dollars) {
        import control.LoginManager._

        currentUser.newLeague(name, cash)

        /* FIXME: this sucks. */
        S.redirectTo("/league-manager?" + HQS.buildQuery(Map("league" -> name), "UTF-8"))
    }

    lazy val cash_f = new DollarsField(League.defaultStartingCash.no$) with FieldErrorRender
    lazy val name_f = new StringField("") with FieldErrorRender

    lazy val submit = Submit(form, "Create") { case NewLeague(name, cash) =>
        import control.LoginManager._

        try {
            createLeague(name, cash)
        } catch {
            case NonPositiveDollars =>
                throw BadFieldInput(cash_f, "Must start with more than $0.00")
            case NameInUse  =>
                throw BadFieldInput(name_f, "Name already taken")
            case NotLoggedIn =>
                throw BadInput("You must be logged in to create a League")
        }
    }
}

object LeagueCreator {
    case class NewLeague(name: String, cash: Dollars)
}
