
// Written by: Owen Healy

package formats;

import org.apache.log4j.spi.ThrowableRenderer;
import java.util.*;

public class ShortThrowableRenderer implements ThrowableRenderer {
    public String[] doRender(Throwable thrown) {
        ArrayList<String> lines = new ArrayList<String>();
        lines.add("\033[31m" + thrown.toString() + "\033[0m");
            
        StackTraceElement[] frames = thrown.getStackTrace();
        for (int i=0; i<frames.length && i<10; i++) {
            lines.add(String.format("    %s %3d",
                frames[i].getFileName(),
                frames[i].getLineNumber()
            ));
        }
        
        String[] aLines = new String[lines.size()];
        for (int i=0; i<aLines.length; i++) {
            aLines[i] = lines.get(i);
        }
        
        return aLines;
    }
}

