# the plain log echoes the program lines

    Code
      cat(r$log, sep = "\n")
    Output
      1          data work.a;
      2            x = 1;
      3            put x=;
      4          run;
      
      x=1
      NOTE: The data set WORK.A has 1 observations and 1 variables.
      5          data work.b;
      6            x = nosuchfn(2);
                       ________
                       68
      ERROR 68-185: The function NOSUCHFN is unknown, or cannot be accessed.
      
      7          run;
      
      NOTE: sasonr stopped processing this step because of errors.
      NOTE: Due to ERROR(s) above, sasonr set option OBS=0, enabling syntax check mode.
            This prevents execution of subsequent data modification statements.
      WARNING: The data set WORK.B may be incomplete.  When this step was stopped there were 0 observations and 1 variables.
      8          data c;
      9            input v;
      10           datalines;
      
      NOTE: The data set WORK.C has 0 observations and 1 variables.
      13         ;
      14         run;
      15         %put done;
      done

# the ANSI view colours the messages under a summary line

    Code
      cat(sas_log(r, "ansi"), sep = "\n")
    Output
      [1m[31m1 ERROR[39m, [32m1 WARNING[39m[22m
      1          data work.a;
      2            x = 1;
      3            put x=;
      4          run;
      
      x=1
      [34mNOTE: The data set WORK.A has 1 observations and 1 variables.[39m
      5          data work.b;
      6            x = nosuchfn(2);
      [31m                 ________[39m
      [31m                 68[39m
      [31mERROR 68-185: The function NOSUCHFN is unknown, or cannot be accessed.[39m
      
      7          run;
      
      [34mNOTE: sasonr stopped processing this step because of errors.[39m
      [34mNOTE: Due to ERROR(s) above, sasonr set option OBS=0, enabling syntax check mode.[39m
      [34m      This prevents execution of subsequent data modification statements.[39m
      [32mWARNING: The data set WORK.B may be incomplete.  When this step was stopped there were 0 observations and 1 variables.[39m
      8          data c;
      9            input v;
      10           datalines;
      
      [34mNOTE: The data set WORK.C has 0 observations and 1 variables.[39m
      13         ;
      14         run;
      15         %put done;
      done

# the HTML view uses the sasonr CSS classes

    Code
      cat(h)
    Output
      <div class="sas-log">
      <p class="sas-log-summary"><span class="sas-error">1 ERROR</span>, <span class="sas-warning">1 WARNING</span></p>
      <details class="sas-step" open><summary>DATA step (line 1)</summary>
      <pre class="sas-log-text"><span class="sas-src">1          data work.a;</span>
      <span class="sas-src">2            x = 1;</span>
      <span class="sas-src">3            put x=;</span>
      <span class="sas-src">4          run;</span>
      <span class="sas-src"></span>
      <span class="sas-put">x=1</span>
      <span class="sas-note">NOTE: The data set WORK.A has 1 observations and 1 variables.</span></pre>
      </details>
      <details class="sas-step" open><summary>DATA step (line 5)</summary>
      <pre class="sas-log-text"><span class="sas-src">5          data work.b;</span>
      <span class="sas-src">6            x = nosuchfn(2);</span>
      <span class="sas-error sas-marker">                 ________</span>
      <span class="sas-error sas-marker">                 68</span>
      <span class="sas-error">ERROR 68-185: The function NOSUCHFN is unknown, or cannot be accessed.</span>
      <span class="sas-src"></span>
      <span class="sas-src">7          run;</span>
      <span class="sas-src"></span>
      <span class="sas-note">NOTE: sasonr stopped processing this step because of errors.</span>
      <span class="sas-note">NOTE: Due to ERROR(s) above, sasonr set option OBS=0, enabling syntax check mode.</span>
      <span class="sas-note">      This prevents execution of subsequent data modification statements.</span>
      <span class="sas-warning">WARNING: The data set WORK.B may be incomplete.  When this step was stopped there were 0 observations and 1 variables.</span></pre>
      </details>
      <details class="sas-step" open><summary>DATA step (line 8)</summary>
      <pre class="sas-log-text"><span class="sas-src">8          data c;</span>
      <span class="sas-src">9            input v;</span>
      <span class="sas-src">10           datalines;</span>
      <span class="sas-src"></span>
      <span class="sas-note">NOTE: The data set WORK.C has 0 observations and 1 variables.</span>
      <span class="sas-src">13         ;</span>
      <span class="sas-src">14         run;</span></pre>
      </details>
      <details class="sas-step" open><summary>Open code</summary>
      <pre class="sas-log-text"><span class="sas-src">15         %put done;</span>
      <span class="sas-put">done</span></pre>
      </details>
      </div>

