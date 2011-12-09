
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
    
    val currentParam = S param "trade"
    
    def render = jsapi.jsFuncDefs ++ refreshable.render
    val refreshable = Refreshable(doRender)
    
    def doRender: NodeSeq = {
        import control.LoginManager._
        import control.PortfolioSwitcher._
        
        try main(currentPortfolio)
        catch { 
            case NotLoggedIn =>
                <div class="block">
                    <p>Login to use auto trades</p>
                </div>
        }
    }
    
    def main(port: Portfolio) = {
        val autoTrades = port.myAutoTrades sortBy (_.title)
        
        if (autoTrades.isEmpty) createButton(port)
        else tradeEditor(port, autoTrades)
    }
    
    def tradeEditor(port: Portfolio, autoTrades: List[AutoTrade]) = {
        val key = java.util.UUID.randomUUID.toString
        
        val current = currentParam flatMap { c =>
            autoTrades filter (_.id == c) headOption
        } openOr autoTrades.head
    
        lazy val tabs = autoTrades map { autoTrade =>
            if (autoTrade == current)
                <div class="tab active">
                    <p>{titleField.main & <input class="blank"/>}</p>
                </div>
            else
                <div class="tab inactive">
                    <p><a href={"?trade="+autoTrade.id}>{autoTrade.title}</a></p>
                </div>
        }
        
        lazy val titleField = StringField(current.title)
        lazy val bodyField = TextAreaField(current.code)
    
        lazy val form: Form[AutoTradeSubmit] = Form(AutoTradeSubmit,
            (
                titleField,
                bodyField
            ),
            (
                <div class="tab-bar">
                    {tabs}
                </div>
                <div class="tab-pane">
                    {bodyField.main &
                        <textarea id={"code-"+key} class="auto-trade-code"/>
                    }
                </div>
                <div class="auto-trade-controls">
                    {executeButton.main}
                    {saveButton.main}
                    {deleteButton.main}
                </div>
            )
        )
        
        lazy val executeButton = Submit(form, "Run", refresh=false) {
            case AutoTradeSubmit(title, code) =>
                current.userModify(title, code)
                logger.info("Executing " + code)
                Call("runAutoTrade", Str(key))
        }
        
        lazy val saveButton = Submit(form, "Save", refresh=false) {
            case AutoTradeSubmit(title, code) =>
                current.userModify(title, code)
        }
        
        lazy val deleteButton = Submit(form, "Delete", refresh=false) { _ =>
            current.userDelete()
            refreshable.refresh
        }
        
        val output =
            <div id={"pane-"+key} class="auto-trade-output">
                <pre id={"output-"+key} class="auto-trade-output">
                
                </pre>
            </div>
        
        val all =
            <div class="block">
                {createButton(port)}
                {form.render}
                {output}
            </div>
            
        all
    }
    
    def createButton(port: Portfolio) =
        FormSubmit.rendered("Create AutoTrade") {
            port.userMakeNewAutoTrade()
            refreshable.refresh
        }

    case class AutoTradeSubmit(
        title: String,
        code: String
    )
}

