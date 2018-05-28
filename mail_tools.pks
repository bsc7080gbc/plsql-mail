CREATE OR REPLACE PACKAGE mail_tools
AS
-- SENDMAIL supports variable message length with/without attachments
-- QUERY_SERVER allows you to check the status of a mail server to see if it is running
-- DUMP_FLATFILE allows you to dump flat file data from query submitted
--
--
-- Query Server to verify that the server is up and running.
-- Connects, Noop Command is executed, Disconnect.

   --
-- GET_MAIL_ADDRESS is utilized in the SENDMAIL procedure
--
--
--
-- Table 100-3 SMTP Reply Codes
-- Reply Code
-- Meaning
--
-- 211 System status, or system help reply
--
-- 214 Help message [Information on how to use the receiver or the meaning of a particular non-standard command; this reply is useful only to the human user]
--
-- 220 <domain> Service ready
--
-- 221 <domain> Service closing transmission channel
--
-- 250 Requested mail action okay, completed
--
-- 251 User not local; will forward to <forward-path>
--
-- 252 OK, pending messages for node <node> started. Cannot VRFY user (e.g., info is not local), but will take message for this user and attempt delivery.
--
-- 253 OK, <messages> pending messages for node <node> started
--
-- 354 Start mail input; end with <CRLF>.<CRLF>
--
-- 355 Octet-offset is the transaction offset
--
-- 421 <domain> Service not available, closing transmission channel (This may be a reply to any command if the service knows it must shut down.)
--
-- 450 Requested mail action not taken: mailbox unavailable [for example, mailbox busy]
--
-- 451 Requested action aborted: local error in processing
--
-- 452 Requested action not taken: insufficient system storage
--
-- 453 You have no mail.
--
-- 454 TLS not available due to temporary reason. Encryption required for requested authentication mechanism.
--
-- 458 Unable to queue messages for node <node>
--
-- 459 Node <node> not allowed: reason
--
-- 500 Syntax error, command unrecognized (This may include errors such as command line too long.)
--
-- 501 Syntax error in parameters or arguments
--
-- 502 Command not implemented
--
-- 503 Bad sequence of commands
--
-- 504 Command parameter not implemented
--
-- 521 <Machine> does not accept mail.
--
-- 530 Must issue a STARTTLS command first. Encryption required for requested authentication mechanism.
--
-- 534 Authentication mechanism is too weak.
--
-- 538 Encryption required for requested authentication mechanism.
--
-- 550 Requested action not taken: mailbox unavailable [for , mailbox not found, no access]
--
-- 551 User not local; please try <forward-path>
--
-- 552 Requested mail action aborted: exceeded storage allocation
--
-- 553 Requested action not taken: mailbox name not allowed [for example, mailbox syntax incorrect]
--
-- 554 Transaction failed
--
--
--
/*
 This version allows for a customized seperator value. Using this function will allow you to
 perform fixed width flat files by defining '' for no seperator and then RPAD/LPAD your columns as necessary.
 Or use whatever seperator you wish to use, pipe, space, zeros, etc.
 */

   --  Example : This will generate a flat file which tabbed seperated
--
-- DECLARE
--    l_rows   NUMBER;
--    l_sql    VARCHAR2(32000);
-- BEGIN
--    l_sql := '
-- SELECT   rpad(hou.NAME,70) udn_desc
-- ,        rpad(pcak.segment1,6) coid
-- ,        rpad(pcak.segment2,4) udn
--     FROM hr_all_organization_units hou, hr.pay_cost_allocation_keyflex pcak
--    WHERE TRUNC (SYSDATE) BETWEEN hou.date_from
--                              AND NVL (hou.date_to, ''31-DEC-4712'')
--      AND pcak.cost_allocation_keyflex_id = hou.cost_allocation_keyflex_id
-- GROUP BY pcak.segment1, pcak.segment2, hou.NAME
-- ORDER BY 1, 2, 3
-- ';
--
--    l_rows :=
--       dump_flatfile
--          (p_query          =>
-- ,         p_dir            => '/xfer'
-- ,         p_filename       => 'test.csv'
-- ,       p_separator     => '     ' -- <= tabbed 5 spaces between each column
-- ,       p_max_linesize   => 32000
-- ,       p_mode       => 'w' -- (w)rite mode or (a)ppend mode
--          );
-- END;
   FUNCTION dump_flatfile (
      p_query                    IN       VARCHAR2
,     p_dir                      IN       VARCHAR2
,     p_filename                 IN       VARCHAR2
,     p_separator                IN       VARCHAR2
,     p_headers                  IN       BOOLEAN DEFAULT FALSE
,     p_trailing_separator       IN       BOOLEAN DEFAULT FALSE
,     p_max_linesize             IN       NUMBER DEFAULT 32000
,     p_mode                     IN       VARCHAR2 DEFAULT 'w' )
      RETURN NUMBER;

   FUNCTION get_mail_address (
      addr_list                  IN OUT   VARCHAR2 )
      RETURN VARCHAR2;

   FUNCTION smtp_command (
      command                    IN       VARCHAR2
,     ok                         IN       VARCHAR2 DEFAULT '250'
,     code                       OUT      VARCHAR2
,     DEBUG                               NUMBER DEFAULT 0 )
      RETURN BOOLEAN;

   FUNCTION query_server (
      smtp_server                         VARCHAR2
,     smtp_server_port                    PLS_INTEGER DEFAULT 25
,     DEBUG                               NUMBER DEFAULT 0 )
      RETURN BOOLEAN;

/*
This procedure uses the UTL_TCP package to send an email message.
Up to three file names may be specified as attachments.

Written: Dave Wotton, 14/6/01 (Cambridge UK)
This script comes with no warranty or support. You are free to
modify it as you wish, but please retain an acknowledgement of
my original authorship.

Amended: Dave Wotton, 10/7/01
Now uses the utl_smtp.write_data() method to send the message,
eliminating the 32Kb message size constraint imposed by the
utl_smtp.data() procedure.

Amended: Dave Wotton, 20/7/01
Increased the v_line variable, which holds the file attachment
lines from 400 to 1000 bytes. This is the maximum supported
by RFC2821, The Simple Mail Transfer Protocol specification.

Amended: Dave Wotton, 24/7/01
Now inserts a blank line before each MIME boundary line. Some
mail-clients require this.

Amended: Dave Wotton, 4/10/01
Introduced a 'debug' parameter. Defaults to 0. If set to
non-zero then errors in opening files for attaching are
reported using dbms_output.put_line.
Include code to hand MS Windows style pathnames.

Amended: Barry Chase, 4/29/03
Added Priority to procedure and also X-Mailer ID.
Removed restrictions for email size limitation as well.
Emails are now formatted text messages, meaning you can
write your message in html format.
And finally, changed from using UTL_SMTP to UTL_TCP instead.

Amended: Barry Chase 11/10/2003
Added session timeout of 4 minutes to prevent hanging server connections

Amended: Barry Chase 12/04/2003
Added Date String so that it represents timezone of originating server
p_datestring

Amended: Barry Chase 03/01/2004
Added functionality to support binary attachments and remote attachments.
Its about 98% complete. Not work perfectly yet. Still trying to figure out
encoding to base64 or mime. Have a good start on it though.

04/12/2004
BCHASE :: Binary Support is fully functional now.

09/01/2005
BCHASE :: Modified attachment directories to use DBA_DIRECTORIES instead
of UTL_DIR in the Oracle initialization file.

02/22/2006
BCHASE :: Added variable length message email support (CLOB)

04/21/2006
BCHASE :: Expanded functionality to include Cc and Bcc
Also removed redundant calls from package. The single
mail_files command will handle flat files and binary files such as zip/pdf/etc.

SMTP Server and SMTP Server Port are parameters on the sendmail procedure now
as well.

Refer to http://home.clara.net/dwotton/dba/oracle_smtp.htm for more
details on the original source code.

For information on the enhanced mail_tools package as provided by Barry
Chase, refer to http://www.myoracleportal.com

*/

   /* Retrieves local binary file from database server.
    * using DBMS_LOB commands and stores into BLOB
    *
    * return BLOB
   */
   FUNCTION get_local_binary_data (
      p_dir                      IN       VARCHAR2
,     p_file                     IN       VARCHAR2 )
      RETURN BLOB;

/* Supports binary attachments and message of variable length. Uses CLOB.*/

-- DECLARE
-- t_blob BLOB;
-- BEGIN
--
-- Use the get_local_binary_data to collect your BLOB from the filesystem
-- or just load from a table where your BLOB is stored at, then just pass
-- as t_blob on the binaryfile parameter below. Remember to provide an
-- appropriate filename. Optionally, you can leave filename NULL and pass
-- the binaryfile parameter as EMPTY_BLOB() to send an email without an
-- attachment.
--
--   t_blob :=
--    mail_tools.get_local_binary_data
--                   ( p_dir =>                         'INTF0047_TABLES'
--,                    p_file =>                        'test_file1.csv' );
--    mail_tools.sendmail
--             ( smtp_server =>                   'your.smtp.server'
-- ,             smtp_server_port =>              25
-- ,             from_name =>                     'Email Address of Sender'
-- ,             to_name =>                       'list of TO email addresses separated by commas (,)'
-- ,             cc_name =>                       'list of CC email addresses separated by commas (,)'
-- ,             bcc_name =>                      'list of BCC email addresses separated by commas (,)'
-- ,             subject =>                       'Some brief Subject'
-- ,             MESSAGE =>                       'Your message goes here. Can include HTML code.'
-- ,             priority =>                      '1-5 1 being the highest priority and 3 normal priority'
-- ,             filename =>                      'your.filename.txt or leave NULL'
-- ,             binaryfile =>                    'your blob is passed here otherwise leave as EMPTY_BLOB()
-- ,             DEBUG =>                         'Default is DBMS output otherwise pass a 1 to disable );
-- END;
--
   PROCEDURE sendmail (
      smtp_server                         VARCHAR2
,     smtp_server_port                    PLS_INTEGER DEFAULT 25
,     from_name                           VARCHAR2
,     to_name                             VARCHAR2
,     cc_name                             VARCHAR2 DEFAULT NULL
,     bcc_name                            VARCHAR2 DEFAULT NULL
,     subject                             VARCHAR2
,     MESSAGE                             CLOB
,     priority                            PLS_INTEGER DEFAULT NULL
,     filename                            VARCHAR2 DEFAULT NULL
,     binaryfile                          BLOB DEFAULT EMPTY_BLOB ( )
,     DEBUG                               NUMBER DEFAULT 0 );

   v_parm_value                  VARCHAR2 ( 4000 );
   lbok                          BOOLEAN;
   v_smtp_server                 VARCHAR2 ( 50 );
   v_smtp_server_port            NUMBER := 25;
   crlf                          VARCHAR2 ( 10 ) := utl_tcp.crlf;
   conn                          utl_tcp.connection;
   p_debug_marker                PLS_INTEGER := 0;
   rc                            INTEGER;
   p_from_name                   VARCHAR2 ( 100 );
   p_to_name                     VARCHAR2 ( 4000 );
   p_cc_name                     VARCHAR2 ( 4000 );
   p_bcc_name                    VARCHAR2 ( 4000 );
   p_subject                     VARCHAR2 ( 150 );
   tx_timeout                    PLS_INTEGER := 240;
                                                  -- 240 Seconds (4 minutes);
--
   p_datestring                  VARCHAR2 ( 100 )
      :=    'Date: '
         || TO_CHAR ( SYSDATE, 'MM/DD/RR HH:MI AM' )
         || ' '
         || DBTIMEZONE
         || ' '
         || '(GMT'
         || DBTIMEZONE
         || ')';
   -- Customize the signature that will appear in the email's MIME header.
   -- Useful for versioning.
   mailer_id            CONSTANT VARCHAR2 ( 256 ) := 'Mailer by Oracle UTL_TCP';
   max_base64_line_width CONSTANT PLS_INTEGER := 76 / 4 * 3;
END;
/

CREATE OR REPLACE PACKAGE BODY mail_tools
IS
   PROCEDURE print_output (
      p_message                  IN       VARCHAR2 )
   IS
   BEGIN
      dbms_output.put_line ( SUBSTR ( p_message
,                                     1
,                                     250 ));

      IF LENGTH ( p_message ) > 250
      THEN
         dbms_output.put_line ( SUBSTR ( p_message
,                                        251
,                                        500 ));
      END IF;

      IF LENGTH ( p_message ) > 500
      THEN
         dbms_output.put_line ( SUBSTR ( p_message
,                                        501
,                                        750 ));
      END IF;

      IF LENGTH ( p_message ) > 750
      THEN
         dbms_output.put_line ( SUBSTR ( p_message
,                                        751
,                                        1000 ));
      END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         NULL;               -- Ignore errors... protect buffer overflow's etc.
   END print_output;

   FUNCTION dump_flatfile (
      p_query                    IN       VARCHAR2
,     p_dir                      IN       VARCHAR2
,     p_filename                 IN       VARCHAR2
,     p_separator                IN       VARCHAR2
,     p_headers                  IN       BOOLEAN DEFAULT FALSE
,     p_trailing_separator       IN       BOOLEAN DEFAULT FALSE
,     p_max_linesize             IN       NUMBER DEFAULT 32000
,     p_mode                     IN       VARCHAR2 DEFAULT 'w' )
      RETURN NUMBER
   IS
      l_output                      utl_file.file_type;
      l_thecursor                   INTEGER DEFAULT dbms_sql.open_cursor;
      l_columnvalue                 VARCHAR2 ( 4000 );
      l_status                      INTEGER;
      l_colcnt                      NUMBER DEFAULT 0;
      l_cnt                         NUMBER DEFAULT 0;
      l_separator                   VARCHAR2 ( 10 ) DEFAULT '';
      l_line                        LONG;
      l_desctbl                     dbms_sql.desc_tab;
      v_sqlerrm                     VARCHAR2 ( 32000 );
      l_mode                        CHAR ( 1 ) := 'w';
   BEGIN
      IF p_mode NOT IN ( 'w', 'a' )
      THEN
         l_mode := 'w';
      ELSE
         l_mode := p_mode;
      END IF;

      l_output := utl_file.fopen ( p_dir
,                                  p_filename
,                                  l_mode
,                                  p_max_linesize );
      dbms_sql.parse ( l_thecursor
,                      p_query
,                      dbms_sql.native );
      dbms_sql.describe_columns ( l_thecursor
,                                 l_colcnt
,                                 l_desctbl );

      FOR i IN 1 .. l_colcnt
      LOOP
         dbms_sql.define_column ( l_thecursor
,                                 i
,                                 l_columnvalue
,                                 4000 );

         IF ( l_desctbl ( i ).col_type = 2 )                   /* number type */
         THEN
            l_desctbl ( i ).col_max_len := l_desctbl ( i ).col_precision + 2;
         ELSIF ( l_desctbl ( i ).col_type = 12 )                 /* date type */
         THEN
/* length of my date format */
            l_desctbl ( i ).col_max_len := 20;
         ELSIF ( l_desctbl ( i ).col_type = 8 )                  /* LONG type */
         THEN
            l_desctbl ( i ).col_max_len := 2000;
         END IF;

         IF p_headers
         THEN
            utl_file.put ( l_output, l_separator || l_desctbl ( i ).col_name );
            l_separator := p_separator;
         END IF;
      END LOOP;

      IF p_trailing_separator
      THEN
         utl_file.put ( l_output, l_separator );
      END IF;

      IF p_headers
      THEN
         utl_file.new_line ( l_output );
      END IF;

      l_status := dbms_sql.EXECUTE ( l_thecursor );

      LOOP
         EXIT WHEN ( dbms_sql.fetch_rows ( l_thecursor ) <= 0 );
         l_line := NULL;
         l_separator := '';

         FOR i IN 1 .. l_colcnt
         LOOP
            dbms_sql.COLUMN_VALUE ( l_thecursor
,                                   i
,                                   l_columnvalue );

            IF NVL ( INSTR ( l_columnvalue, ',' ), 0 ) = 0
            THEN
               NULL;
            ELSE
               l_columnvalue := '"' || l_columnvalue || '"';
            END IF;

            utl_file.put ( l_output, l_separator || l_columnvalue );
            l_separator := p_separator;
         END LOOP;

         IF p_trailing_separator
         THEN
            utl_file.put ( l_output, l_separator );
         END IF;

         utl_file.new_line ( l_output );
         l_cnt := l_cnt + 1;
      END LOOP;

      dbms_sql.close_cursor ( l_thecursor );
      utl_file.fclose ( l_output );
      RETURN l_cnt;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         dbms_output.put_line ( 'NO_DATA_FOUND' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN utl_file.invalid_path
      THEN
         dbms_output.put_line ( 'UTL_FILE.INVALID_PATH' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN utl_file.read_error
      THEN
         dbms_output.put_line ( 'UTL_FILE.READ_ERROR' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN utl_file.write_error
      THEN
         dbms_output.put_line ( 'UTL_FILE.WRITE_ERROR' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN utl_file.invalid_mode
      THEN
         dbms_output.put_line ( 'UTL_FILE.INVALID_MODE' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN utl_file.invalid_filehandle
      THEN
         dbms_output.put_line ( 'UTL_FILE.INVALID_FILEHANDLE' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN utl_file.invalid_operation
      THEN
         dbms_output.put_line ( 'UTL_FILE.INVALID_OPERATION' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN utl_file.internal_error
      THEN
         dbms_output.put_line ( 'UTL_FILE.INTERNAL_ERROR' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN utl_file.invalid_maxlinesize
      THEN
         dbms_output.put_line ( 'UTL_FILE.INVALID_MAXLINESIZE' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN VALUE_ERROR
      THEN
         dbms_output.put_line ( 'UTL_FILE.VALUE_ERROR' );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
      WHEN OTHERS
      THEN
         DBMS_OUTUT.PUT_LINE ( SUBSTR('ERROR in dump_csv : '||SQLCODE || ' - ' || SQLERRM,1,255 );
         utl_file.fclose ( l_output );
         RETURN l_cnt;
   END dump_flatfile;

   -- Return the next email address in the list of email addresses, separated
   -- by either a "," or a ";".  The format of mailbox may be in one of these:
   --   someone@some-domain
   --   "Someone at some domain" <someone@some-domain>
   --   Someone at some domain <someone@some-domain>
   FUNCTION get_mail_address (
      addr_list                  IN OUT   VARCHAR2 )
      RETURN VARCHAR2
   IS
      addr                          VARCHAR2 ( 256 );
      i                             PLS_INTEGER;

      FUNCTION lookup_unquoted_char (
         str                        IN       VARCHAR2
,        chrs                       IN       VARCHAR2 )
         RETURN PLS_INTEGER
      AS
         c                             VARCHAR2 ( 5 );
         i                             PLS_INTEGER;
         len                           PLS_INTEGER;
         inside_quote                  BOOLEAN;
      BEGIN
         inside_quote := FALSE;
         i := 1;
         len := LENGTH ( str );

         WHILE ( i <= len )
         LOOP
            c := SUBSTR ( str
,                         i
,                         1 );

            IF ( inside_quote )
            THEN
               IF ( c = '"' )
               THEN
                  inside_quote := FALSE;
               ELSIF ( c = '\' )
               THEN
                  i := i + 1;
               -- Skip the quote character
               END IF;

               GOTO next_char;
            END IF;

            IF ( c = '"' )
            THEN
               inside_quote := TRUE;
               GOTO next_char;
            END IF;

            IF ( INSTR ( chrs, c ) >= 1 )
            THEN
               RETURN i;
            END IF;

            <<next_char>>
            i := i + 1;
         END LOOP;

         RETURN 0;
      END;
   BEGIN
      addr_list := LTRIM ( addr_list );
      i := lookup_unquoted_char ( addr_list, ',;' );

      IF ( i >= 1 )
      THEN
         addr := SUBSTR ( addr_list
,                         1
,                         i - 1 );
         addr_list := SUBSTR ( addr_list, i + 1 );
      ELSE
         addr := addr_list;
         addr_list := '';
      END IF;

      i := lookup_unquoted_char ( addr, '<' );

      IF ( i >= 1 )
      THEN
         addr := SUBSTR ( addr, i + 1 );
         i := INSTR ( addr, '>' );

         IF ( i >= 1 )
         THEN
            addr := SUBSTR ( addr
,                            1
,                            i - 1 );
         END IF;
      END IF;

      RETURN addr;
   END;

   FUNCTION smtp_command (
      command                    IN       VARCHAR2
,     ok                         IN       VARCHAR2 DEFAULT '250'
,     code                       OUT      VARCHAR2
,     DEBUG                               NUMBER DEFAULT 0 )
      RETURN BOOLEAN
   IS
      response                      VARCHAR2 ( 3 );
      p_output_message              VARCHAR2 ( 255 );
      len                           PLS_INTEGER;
      PRAGMA AUTONOMOUS_TRANSACTION;
   BEGIN
      len := utl_tcp.write_line ( conn, command );
      p_output_message := SUBSTR ( utl_tcp.get_line ( conn, TRUE )
,                                  1
,                                  255 );
      response := SUBSTR ( p_output_message
,                          1
,                          3 );
      p_output_message :=
                         SUBSTR ( command || ' - ' || p_output_message
,                                 1
,                                 255 );

      IF DEBUG = 1
      THEN                                                          -- No Output
         NULL;
      ELSE                                          -- Then DBMS_OUTPUT messages
         print_output ( p_output_message );
      END IF;

      IF ( response <> ok )
      THEN
         code := response;
         RETURN FALSE;
      ELSE
         code := response;
         RETURN TRUE;
      END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         p_output_message := SQLCODE || ' - ' || SQLERRM;
         code := p_output_message;
         RETURN FALSE;
   END smtp_command;

   FUNCTION query_server (
      smtp_server                         VARCHAR2
,     smtp_server_port                    PLS_INTEGER DEFAULT 25
,     DEBUG                               NUMBER DEFAULT 0 )
      RETURN BOOLEAN
   IS
      p_output_message              VARCHAR2 ( 255 );
      PRAGMA AUTONOMOUS_TRANSACTION;
      err_noop                      EXCEPTION;    -- SMTP code 250 not received
      err_server_reject             EXCEPTION;
   -- SMTP code 421 means rejected
   BEGIN
      v_smtp_server := smtp_server;
      v_smtp_server_port := smtp_server_port;
-- Open the SMTP connection ...
-- ------------------------
      conn :=
         utl_tcp.open_connection ( remote_host =>                   v_smtp_server
,                                  remote_port =>                   v_smtp_server_port
,                                  tx_timeout =>                    tx_timeout );
      ----- OPEN SMTP PORT CONNECTION
      rc := utl_tcp.write_line ( conn, 'HELO ' || v_smtp_server );
            -- This will return a 250 OK response if your connection is valid
-- Initial handshaking ...
-- -------------------
      ----- PERFORMS HANDSHAKING WITH SMTP SERVER
      p_output_message := utl_tcp.get_line ( conn, TRUE );

      IF DEBUG = 1
      THEN                                                          -- No Output
         NULL;
      ELSE                                          -- Then DBMS_OUTPUT messages
         print_output ( p_output_message );
      END IF;

      IF SUBSTR ( p_output_message
,                 1
,                 3 ) = '421'
      THEN
         RAISE err_server_reject;
      END IF;

      -- NOOP THE SERVER
      rc := utl_tcp.write_line ( conn, 'NOOP' );
            -- This will return a 250 OK response if your connection is valid
-- Initial handshaking ...
-- -------------------
      ----- PERFORMS NOOP WITH SMTP SERVER
      p_output_message := utl_tcp.get_line ( conn, TRUE );

      IF DEBUG = 1
      THEN                                                          -- No Output
         NULL;
      ELSE                                          -- Then DBMS_OUTPUT messages
         print_output ( p_output_message );
      END IF;

      IF SUBSTR ( p_output_message
,                 1
,                 3 ) <> '250'
      THEN
         RAISE err_noop;
      END IF;

      rc := utl_tcp.write_line ( conn, 'QUIT' );

      ----- ENDS EMAIL TRANSACTION
      BEGIN
         FOR i_idx IN 1 .. 100
         LOOP
            p_output_message := utl_tcp.get_line ( conn, TRUE );

            IF DEBUG = 1
            THEN                                                   -- No Output
               NULL;
            ELSE                                    -- Then DBMS_OUTPUT messages
               print_output ( p_output_message );
            END IF;
         END LOOP;
      EXCEPTION
         WHEN OTHERS
         THEN
            IF DEBUG = 1
            THEN                                                   -- No Output
               NULL;
            ELSE                                    -- Then DBMS_OUTPUT messages
               print_output ( p_output_message );
            END IF;
      END;

      utl_tcp.close_connection ( conn );        ----- CLOSE SMTP PORT CONNECTION
      RETURN TRUE;
   EXCEPTION
      WHEN err_server_reject
      THEN
         print_output (    'ERROR -'
                        || ' Server Rejected Connection ::'
                        || ' SERVER_MSG := '
                        || p_output_message );
         RETURN FALSE;
      WHEN err_noop
      THEN
         print_output (    'ERROR -'
                        || ' NOOP Check Failed ::'
                        || ' SERVER_MSG := '
                        || p_output_message );
         utl_tcp.close_connection ( conn );     ----- CLOSE SMTP PORT CONNECTION
         RETURN FALSE;
   END query_server;

   FUNCTION get_local_binary_data (
      p_dir                      IN       VARCHAR2
,     p_file                     IN       VARCHAR2 )
      RETURN BLOB
   IS
-- --------------------------------------------------------------------------
      l_bfile                       BFILE;
      l_data                        BLOB;
      l_dbdir                       VARCHAR2 ( 100 ) := p_dir;
   BEGIN
      dbms_lob.createtemporary ( lob_loc =>                       l_data
,                                CACHE =>                         TRUE
,                                dur =>                           dbms_lob.CALL );
      l_bfile := BFILENAME ( l_dbdir, p_file );
      dbms_lob.fileopen ( l_bfile, dbms_lob.file_readonly );
      dbms_lob.loadfromfile ( l_data
,                             l_bfile
,                             dbms_lob.getlength ( l_bfile ));
      dbms_lob.fileclose ( l_bfile );
      RETURN l_data;
   EXCEPTION
      WHEN OTHERS
      THEN
         print_output (    'Error during GET_LOCAL_BINARY_DATA :: '
                        || SQLCODE
                        || ' - '
                        || SQLERRM );
         dbms_lob.fileclose ( l_bfile );
         RAISE;
   END get_local_binary_data;

------------------------------------------------------------------------
   PROCEDURE attach_base64 (
      conn                       IN OUT NOCOPY utl_tcp.connection
,     DATA                       IN       BLOB )
   IS
      i                             PLS_INTEGER;
      len                           PLS_INTEGER;
      l_result                      PLS_INTEGER;
      l_buffer                      RAW ( 32767 );
      l_pos                         INTEGER := 1;
      l_blob_len                    INTEGER;
      l_amount                      BINARY_INTEGER := 32767;
--
      req                           utl_http.req;
      resp                          utl_http.resp;
      pdata                         RAW ( 200 );
   BEGIN
      -- Split the Base64-encoded attachment into multiple lines

      -- In writing Base-64 encoded text following the MIME format below,
      -- the MIME format requires that a long piece of data must be splitted
      -- into multiple lines and each line of encoded data cannot exceed
      -- 80 characters, including the new-line characters. Also, when
      -- splitting the original data into pieces, the length of each chunk
      -- of data before encoding must be a multiple of 3, except for the
      -- last chunk. The constant MAX_BASE64_LINE_WIDTH
      -- (76 / 4 * 3 = 57) is the maximum length (in bytes) of each chunk
      -- of data before encoding.
--
      l_blob_len := dbms_lob.getlength ( DATA );

--
--
--
--
      WHILE l_pos < l_blob_len
      LOOP
         l_amount := max_base64_line_width;
         dbms_lob.READ ( DATA
,                        l_amount
,                        l_pos
,                        l_buffer );
         rc := utl_tcp.write_raw ( conn, utl_encode.base64_encode ( l_buffer ));
         utl_tcp.FLUSH ( conn );
         l_pos := l_pos + max_base64_line_width;
         rc := utl_tcp.write_line ( conn, crlf );
      END LOOP;
   END attach_base64;

   PROCEDURE sendmail (
      smtp_server                         VARCHAR2
,     smtp_server_port                    PLS_INTEGER DEFAULT 25
,     from_name                           VARCHAR2
,     to_name                             VARCHAR2
,     cc_name                             VARCHAR2 DEFAULT NULL
,     bcc_name                            VARCHAR2 DEFAULT NULL
,     subject                             VARCHAR2
,     MESSAGE                             CLOB
,     priority                            PLS_INTEGER DEFAULT NULL
,     filename                            VARCHAR2 DEFAULT NULL
,     binaryfile                          BLOB DEFAULT EMPTY_BLOB ( )
,     DEBUG                               NUMBER DEFAULT 0 )
   IS
--
      pos                           PLS_INTEGER := 1;
      bytes_o_data         CONSTANT PLS_INTEGER := 32767;
      offset                        PLS_INTEGER := bytes_o_data;
      msg_length           CONSTANT PLS_INTEGER
                                              := dbms_lob.getlength ( MESSAGE );
--
      v_line                        VARCHAR2 ( 32767 );
      i                             BINARY_INTEGER;
      v_slash_pos                   NUMBER;
      my_recipients                 VARCHAR2 ( 32767 );
      p_recipient_count             PLS_INTEGER := 0;
      p_output_message              VARCHAR2 ( 2000 );
      PRAGMA AUTONOMOUS_TRANSACTION;
      err_server_reject             EXCEPTION;
      -- SMTP code 421 means rejected
      err_message_send              EXCEPTION;         -- SMTP code must be 250
      err_end_of_input              EXCEPTION;
   -- Used to signify last line of input retrieved
--
--
      l_result                      PLS_INTEGER;
      l_buffer_b                    RAW ( 32767 );
      l_amount                      BINARY_INTEGER := 32767;
      l_pos                         INTEGER := 1;
      l_blob_len                    INTEGER;
      l_blob                        BLOB;
      g_debug                       BOOLEAN := TRUE;
      i_base64                      PLS_INTEGER;
      len_base64                    PLS_INTEGER;
   BEGIN
      v_smtp_server := smtp_server;
      v_smtp_server_port := smtp_server_port;
      l_blob := binaryfile;
-- Open the SMTP connection ...
-- ------------------------
      conn :=
         utl_tcp.open_connection ( remote_host =>                   v_smtp_server
,                                  remote_port =>                   v_smtp_server_port
,                                  tx_timeout =>                    tx_timeout );
      ----- OPEN SMTP PORT CONNECTION
      rc := utl_tcp.write_line ( conn, 'HELO ' || v_smtp_server );
-- Initial handshaking ...
-- -------------------
      ----- PERFORMS HANDSHAKING WITH SMTP SERVER
      p_output_message := utl_tcp.get_line ( conn, TRUE );

      IF DEBUG = 1
      THEN                                                          -- No Output
         NULL;
      ELSE                                          -- Then DBMS_OUTPUT messages
         print_output ( p_output_message );
      END IF;

      IF SUBSTR ( p_output_message
,                 1
,                 3 ) = '421'
      THEN
         RAISE err_server_reject;
      ELSE
--      DBMS_OUTPUT.put_line (UTL_TCP.get_line (conn, TRUE));
         rc := utl_tcp.write_line ( conn, 'MAIL FROM: ' || from_name );
         ----- MBOX SENDING THE EMAIL
         p_output_message := utl_tcp.get_line ( conn, TRUE );

         IF DEBUG = 1
         THEN                                                      -- No Output
            NULL;
         ELSE                                       -- Then DBMS_OUTPUT messages
            print_output ( p_output_message );
         END IF;

--      DBMS_OUTPUT.put_line (UTL_TCP.get_line (conn, TRUE));

         --      rc := UTL_TCP.write_line (conn, 'RCPT TO: ' || to_name);
         -- Specify recipient(s) of the email.
         my_recipients := to_name;

         WHILE ( my_recipients IS NOT NULL )
         LOOP
            BEGIN
               rc :=
                  utl_tcp.write_line ( conn
,                                         'RCPT TO: '
                                       || get_mail_address ( my_recipients ));
               p_recipient_count := p_recipient_count + 1;
            END;
         END LOOP;

--         DBMS_OUTPUT.put_line ('RCPT TO: COUNT ' || p_recipient_count);
         ----- MBOX RECV THE EMAIL
         p_output_message := utl_tcp.get_line ( conn, TRUE );

         IF DEBUG = 1
         THEN                                                       -- No Output
            NULL;
         ELSE                                       -- Then DBMS_OUTPUT messages
            print_output ( p_output_message );
         END IF;

--      DBMS_OUTPUT.put_line (UTL_TCP.get_line (conn, TRUE));

         --      rc := UTL_TCP.write_line (conn, 'RCPT TO: ' || cc_name);
         -- Specify cc recipient(s) of the email.
         my_recipients := cc_name;

         WHILE ( my_recipients IS NOT NULL )
         LOOP
            BEGIN
               rc :=
                  utl_tcp.write_line ( conn
,                                         'RCPT TO: '
                                       || get_mail_address ( my_recipients ));
               p_recipient_count := p_recipient_count + 1;
            END;
         END LOOP;

--         DBMS_OUTPUT.put_line ('RCPT TO: COUNT ' || p_recipient_count);
         ----- MBOX RECV THE EMAIL
         p_output_message := utl_tcp.get_line ( conn, TRUE );

         IF DEBUG = 1
         THEN                                                       -- No Output
            NULL;
         ELSE                                       -- Then DBMS_OUTPUT messages
            print_output ( p_output_message );
         END IF;

--      DBMS_OUTPUT.put_line (UTL_TCP.get_line (conn, TRUE));

         --      rc := UTL_TCP.write_line (conn, 'RCPT TO: ' || bcc_name);
         -- Specify bcc recipient(s) of the email.
         my_recipients := bcc_name;

         WHILE ( my_recipients IS NOT NULL )
         LOOP
            BEGIN
               rc :=
                  utl_tcp.write_line ( conn
,                                         'RCPT TO: '
                                       || get_mail_address ( my_recipients ));
               p_recipient_count := p_recipient_count + 1;
            END;
         END LOOP;

--         DBMS_OUTPUT.put_line ('RCPT TO: COUNT ' || p_recipient_count);
         ----- MBOX RECV THE EMAIL
         p_output_message := utl_tcp.get_line ( conn, TRUE );

         IF DEBUG = 1
         THEN                                                       -- No Output
            NULL;
         ELSE                                       -- Then DBMS_OUTPUT messages
            print_output ( p_output_message );
         END IF;

--      DBMS_OUTPUT.put_line (UTL_TCP.get_line (conn, TRUE));
         rc := utl_tcp.write_line ( conn, 'DATA' );
         ----- EMAIL MSG BODY START
         p_output_message := utl_tcp.get_line ( conn, TRUE );

         IF DEBUG = 1
         THEN                                                       -- No Output
            NULL;
         ELSE                                       -- Then DBMS_OUTPUT messages
            print_output ( p_output_message );
         END IF;

--      DBMS_OUTPUT.put_line (UTL_TCP.get_line (conn, TRUE));
-- build the start of the mail message ...
-- -----------------------------------
         rc := utl_tcp.write_line ( conn, p_datestring );
         rc := utl_tcp.write_line ( conn, 'From: ' || from_name );
         rc := utl_tcp.write_line ( conn, 'Subject: ' || subject );
         rc := utl_tcp.write_line ( conn, 'To: ' || to_name );

         IF cc_name IS NOT NULL
         THEN
            rc := utl_tcp.write_line ( conn, 'Cc: ' || cc_name );
         END IF;

         IF bcc_name IS NOT NULL
         THEN
            rc := utl_tcp.write_line ( conn, 'Bcc: ' || bcc_name );
         END IF;

         rc := utl_tcp.write_line ( conn, 'Mime-Version: 1.0' );

              -- Set priority:
         --   High      Normal       Low
         --   1     2     3     4     5
         IF ( priority IS NOT NULL )
         THEN
            rc := utl_tcp.write_line ( conn, 'X-Priority: ' || priority );
         END IF;

         rc := utl_tcp.write_line ( conn, 'X-Mailer: ' || mailer_id );
         rc :=
            utl_tcp.write_line
               ( conn
,                'Content-Type: multipart/mixed; boundary="=_mixed 0052287A85256E75_="' );
         rc := utl_tcp.write_line ( conn, '' );
         rc :=
            utl_tcp.write_line
               ( conn
,                'This is a Mime message, which your current mail reader may not' );
         rc :=
            utl_tcp.write_line
               ( conn
,                'understand. Parts of the message will appear as text. If the remainder' );
         rc :=
            utl_tcp.write_line
               ( conn
,                'appears as random characters in the message body, instead of as' );
         rc :=
            utl_tcp.write_line
               ( conn
,                'attachments, then you''ll have to extract these parts and decode them' );
         rc := utl_tcp.write_line ( conn, 'manually.' );
         rc := utl_tcp.write_line ( conn, '' );
         rc := utl_tcp.write_line ( conn, '--=_mixed 0052287A85256E75_=' );
         rc :=
            utl_tcp.write_line ( conn
,                                'Content-Type: text/html; charset=8859-1' );
         rc := utl_tcp.write_line ( conn, '' );
         rc := utl_tcp.write_line ( conn, '<html>' );
         rc := utl_tcp.write_line ( conn, '<head>' );
         rc :=
            utl_tcp.write_line
               ( conn
,                '<meta http-equiv="Content-Type" content="text/html;charset=8859-1">' );
         rc := utl_tcp.write_line ( conn, '<title>' );
         rc := utl_tcp.write_line ( conn, subject );
         rc := utl_tcp.write_line ( conn, '</title>' );
         rc := utl_tcp.write_line ( conn, '</head>' );
         rc := utl_tcp.write_line ( conn, '<body>' );

         WHILE pos < msg_length
         LOOP
            rc :=
               utl_tcp.write_line ( conn
,                                   dbms_lob.SUBSTR ( MESSAGE
,                                                     offset
,                                                     pos ));
            pos := pos + offset;
            offset := LEAST ( bytes_o_data, msg_length - offset );
         END LOOP;

         rc := utl_tcp.write_line ( conn, '<BR><BR>' );
         rc := utl_tcp.write_line ( conn, '</body></html>' );
         rc := utl_tcp.write_line ( conn, '' );
         rc := utl_tcp.write_line ( conn, crlf );

-- Append the file BLOB  ...
-- ----------------
            -- If the filename has been supplied ... it will fail if the BLOB is empty
         IF filename IS NOT NULL
         THEN
            BEGIN
               -- generate the MIME boundary line ...
               rc :=
                    utl_tcp.write_line ( conn, '--=_mixed 0052287A85256E75_=' );
               rc :=
                  utl_tcp.write_line
                          ( conn
,                              'Content-Type: application/octet-stream; name="'
                            || filename
                            || '"' );
               rc :=
                  utl_tcp.write_line
                              ( conn
,                                  'Content-Disposition: attachment; filename="'
                                || filename
                                || '"' );
               rc :=
                  utl_tcp.write_line ( conn
,                                      'Content-Transfer-Encoding: base64' );
               rc := utl_tcp.write_line ( conn, '' );
               rc := utl_tcp.write_line ( conn, '' );
               -- and append the file contents to the end of the message ...

               -- Go get the file and the loop through blob and attach data
               -- and append the file contents to the end of the message ...
               attach_base64 ( conn =>                          conn
,                              DATA =>                          l_blob );
            EXCEPTION
               WHEN OTHERS
               THEN
                  p_output_message :=
                        'Error in attaching file '
                     || filename
                     || ' :: '
                     || SQLCODE
                     || ' - '
                     || SQLERRM;

                  IF DEBUG = 1
                  THEN                                              -- No Output
                     NULL;
                  ELSE                              -- Then DBMS_OUTPUT messages
                     print_output ( p_output_message );
                  END IF;

                  RAISE err_message_send;
            END;
         END IF;

         rc := utl_tcp.write_line ( conn, '' );
-- --
--
         -- append the final boundary line ...
         rc := utl_tcp.write_line ( conn, '' );
         rc := utl_tcp.write_line ( conn, '--=_mixed 0052287A85256E75_=--' );
         rc := utl_tcp.write_line ( conn, '' );
         -- and close the SMTP connection  ...
         rc := utl_tcp.write_line ( conn, '.' );
         ----- EMAIL MESSAGE BODY END
         p_output_message := utl_tcp.get_line ( conn, TRUE );

         IF DEBUG = 1
         THEN                                                       -- No Output
            NULL;
         ELSE                                       -- Then DBMS_OUTPUT messages
            print_output ( p_output_message );
         END IF;

--      DBMS_OUTPUT.put_line (UTL_TCP.get_line (conn, TRUE));
         rc := utl_tcp.write_line ( conn, 'QUIT' );
         ----- ENDS EMAIL TRANSACTION
         p_output_message := utl_tcp.get_line ( conn, TRUE );

         -- Capture '.' Message sent dialog
         IF DEBUG = 1
         THEN                                                       -- No Output
            NULL;
         ELSE                                       -- Then DBMS_OUTPUT messages
            print_output ( p_output_message );
         END IF;

         BEGIN
            FOR i_idx IN 1 .. 100
            LOOP
               p_output_message := utl_tcp.get_line ( conn, TRUE );

               IF DEBUG = 1
               THEN                                                -- No Output
                  NULL;
               ELSE                                 -- Then DBMS_OUTPUT messages
                  print_output ( p_output_message );
               END IF;
            END LOOP;
         EXCEPTION
            WHEN OTHERS
            THEN
               IF DEBUG = 1
               THEN                                                -- No Output
                  NULL;
               ELSE                                 -- Then DBMS_OUTPUT messages
                  print_output ( p_output_message );
               END IF;
         END;
      END IF;                                               -- err_server_reject

      utl_tcp.close_connection ( conn );        ----- CLOSE SMTP PORT CONNECTION
   EXCEPTION
      WHEN err_message_send
      THEN
         print_output (    CHR ( 10 )
                        || CHR ( 10 )
                        || 'ERROR -'
                        || ' Message was not submitted for delivery' );
         print_output ( ' [FROM_NAME := ' || from_name || '] ' );
         print_output ( ' [TO_NAME := ' || to_name || '] ' );
         print_output ( ' [CC_NAME := ' || cc_name || '] ' );
         print_output ( ' [BCC_NAME := ' || bcc_name || '] ' );
         print_output ( ' [SUBJECT := ' || subject || '] ' );
         print_output ( ' SERVER_MSG := ' || p_output_message );
         utl_tcp.close_connection ( conn );     ----- CLOSE SMTP PORT CONNECTION
      WHEN err_server_reject
      THEN
         print_output (    CHR ( 10 )
                        || CHR ( 10 )
                        || 'ERROR -'
                        || ' Server Rejected Email' );
         print_output ( ' [FROM_NAME := ' || from_name || '] ' );
         print_output ( ' [TO_NAME := ' || to_name || '] ' );
         print_output ( ' [CC_NAME := ' || cc_name || '] ' );
         print_output ( ' [BCC_NAME := ' || bcc_name || '] ' );
         print_output ( ' [SUBJECT := ' || subject || '] ' );
         print_output ( ' SERVER_MSG := ' || p_output_message );
      WHEN OTHERS
      THEN
         print_output (    CHR ( 10 )
                        || CHR ( 10 )
                        || 'ERROR :: '
                        || SQLCODE
                        || ' - '
                        || SQLERRM );
         print_output ( ' [FROM_NAME := ' || from_name || '] ' );
         print_output ( ' [TO_NAME := ' || to_name || '] ' );
         print_output ( ' [CC_NAME := ' || cc_name || '] ' );
         print_output ( ' [BCC_NAME := ' || bcc_name || '] ' );
         print_output ( ' [SUBJECT := ' || subject || '] ' );
         print_output ( ' SERVER_MSG := ' || p_output_message );
   END sendmail;
END;
/

