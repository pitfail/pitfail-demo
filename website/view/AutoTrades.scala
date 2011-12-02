
package code
package snippet

import net.liftweb.{common, http, util}
import common._
import util._
import http._
import scala.xml._

import intform._
import formats._

import http._
import http.js._
import JsCmds._
import JE._

import model._
import model.schema._

class AutoTrades extends Page with Loggable {
    
    def render = jsapi.jsFuncDefs ++ refreshable.render
    val refreshable = Refreshable(doRender)
    
    def doRender: NodeSeq = {
        import control.LoginManager._
        
        try main(currentUser.mainPortfolio)
        catch { 
            case NotLoggedIn =>
                <p>Login to use auto trades</p>
        }
    }
    
    def main(port: Portfolio) = {
        lazy val content =
            <div class="block">
                <p>{newAutoTrade}</p>
                {autoTradeList}
            </div>
        
        lazy val newAutoTrade = FormSubmit.rendered("Make new AutoTrade") {
            port.userMakeNewAutoTrade()
            refreshable.refresh
        }
        
        lazy val autoTradeList = port.myAutoTrades map renderAutoTrade _
        
        content
    }
    
    def renderAutoTrade(trade: AutoTrade) = {
        val key = java.util.UUID.randomUUID.toString
        
        val title = trade.title
        val code  = trade.code
        
        lazy val form: Form[AutoTradeSubmit] = Form(AutoTradeSubmit,
            (
                titleField,
                bodyField
            ),
            <div class="auto-trade-form">
                <div class="auto-trade-title">{titleField.main} {titleField.errors}</div>
                <div class="auto-trade-body">{bodyField.main & <textarea id={"code-"+key}/>}</div>
                <div class="auto-trade-controls">
                    {saveButton.main} {saveButton.errors}
                    {executeButton.main} {executeButton.errors}
                </div>
            </div>
        )
        
        lazy val titleField = StringField(title)
        lazy val bodyField = TextAreaField(code)
        
        lazy val saveButton = Submit(form, "Save", refresh=false) { result =>
            trade.userModify(result.title, result.code)
        }
        
        lazy val executeButton = Submit(form, "Run", refresh=false) { result =>
            trade.userModify(result.title, result.code)
            logger.info("Executing " + result.code)
            Call("runAutoTrade", Str(key))
        }
        
        lazy val outputPane = {
            <pre id={"output-"+key} class="auto-trade-output">Stuff goes here</pre>
        }
        
        <div class="auto-trade">
            <table class="auto-trade-table">
                <col class="auto-trade-left-col"/>
                <col class="auto-trade-right-col"/>
                <tr>
                    <td>Code:</td>
                    <td>Output:</td>
                </tr>
                <tr>
                    <td class="auto-trade-left">{form.render}</td>
                    <td class="auto-trade-right">{outputPane}</td>
                </tr>
            </table>
        </div>
    }
    
    case class AutoTradeSubmit(
        title: String,
        code: String
    )
}

