
package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import model.schema;
import static model.schema.*;
import model.*;
import scala.collection.*;
import static scala.collection.JavaConversions.*;
import java.util.*;
import scala.math.BigDecimal;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.sql.DriverManager;
import scala.math.*;
import com.google.gson.*;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class LeagueTestServlet extends HttpServlet {
    
    @Override
    public void doGet(
        HttpServletRequest request,
        HttpServletResponse response
    )
        throws ServletException, IOException
    {
        try {
            System.err.println("Running LeagueeTestServlet");
            
            PrintWriter out = response.getWriter();
            
            // Get a user
            UserSchema.User user = operations.getUser("ellbur_k_a");
            
            // List the portfolios (teams) a user belongs to
            List<UserSchema.Portfolio> portfolios = user.getPortfolios();
            for (UserSchema.Portfolio p : portfolios) {
                out.printf("Portfolio %s\n", p.name());
            }
            
            // Get the current portfolio
            UserSchema.Portfolio current = user.getCurrentPortfolio();
            out.printf("Current portfolio is %s\n", current.name());
            
            // Get assets from the portfolio
            double cash = current.cash().dollars().doubleValue();
            out.printf("Cash: $%.2f\n", cash);
            for (StockSchema.StockAsset asset : current.getMyStockAssets()) {
                BigDecimal shares  = asset.shares().shares();
                BigDecimal price   = asset.price().price();
                BigDecimal dollars = asset.dollars().dollars();
                String ticker  = asset.ticker();
                
                out.printf("%d shares ($%.2f) of %s at $%.2f/sh\n",
                    shares.intValue(),
                    dollars.doubleValue(),
                    ticker,
                    price.doubleValue()
                );
            }
            
            // Get the league the portfolio belongs to
            UserSchema.League league = current.getLeague();
            
            // Get the 10 highest portfolios for the league
            List<UserSchema.Portfolio> leaders = league.getLeaders(10);
            for (UserSchema.Portfolio p : leaders) {
                out.printf("#%d %s\n", p.rank(), p.name());
            }
            
            // Get the "default" league
            UserSchema.League defaultLeague = operations.getDefaultLeague();
            out.printf("Default league is %s\n", defaultLeague.name());
            
            // Create a new portfolio (team)
            try {
                UserSchema.Portfolio newPort = user.userCreatePortfolio("kmfe");
                
                // Invite someone to it
                try {
                    newPort.userInviteUser("sonu_pillai");
                }
                catch (SchemaErrors$NoSuchUser$ e) {
                    out.printf("No user named sonu_pillai\n");
                }
            }
            catch (SchemaErrors$NameInUse$ e) {
                out.printf("Name kmfe is already in use\n");
            }
            
            // Create a new league
            try {
                user.userCreateLeague("6203", new Dollars("500000"));
            }
            catch (SchemaErrors$NameInUse$ e) {
                out.printf("Name 6203 is already taken\n");
            }
        }
        catch (RuntimeException e) {
            e.printStackTrace();
            throw e;
        }
    }
    
    @Override
    public void init() throws ServletException { }
    
    @Override
    public void destroy() { }
    
    @Override
    public void doPost(
        HttpServletRequest request,
        HttpServletResponse response
    ) throws ServletException, IOException { }
}


