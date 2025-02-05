;; ---------------------------------------------------------------
;; Copyright © 2022, 2023, 2025 Jason J.A. Stephenson <jason@sigio.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; ---------------------------------------------------------------

;; A library of my skeletons.

(require 'skeleton)
;; First, some code stolen from EmacsWiki
;; (https://www.emacswiki.org/emacs/SkeletonMode#h5o-15) to make
;; navigating in skeletons easier.
(defvar skeleton-markers nil
  "Markers for locations saved in skeleton-positions")

(add-hook 'skeleton-end-hook 'skeleton-make-markers)

(defun skeleton-make-markers ()
  (while skeleton-markers
    (set-marker (pop skeleton-markers) nil))
  (setq skeleton-markers
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defun skeleton-next-position (&optional reverse)
  "Jump to next position in skeleton.
         REVERSE - Jump to previous position in skeleton"
  (interactive "P")
  (let* ((positions (mapcar 'marker-position skeleton-markers))
         (positions (if reverse (reverse positions) positions))
         (comp (if reverse '> '<))
         pos)
    (when positions
      (if (catch 'break
            (while (setq pos (pop positions))
              (when (funcall comp (point) pos)
                (throw 'break t))))
          (goto-char pos)
        (goto-char (marker-position
                    (car skeleton-markers)))))))
;; End of stolen code.

;; Use C-% for 'skeleton-next-position.
(global-set-key [?\C-%] 'skeleton-next-position)

(define-skeleton guile-scheme
  "Makes a basic skeleton for a scheme script to run via guile."
  nil
  "#!/usr/bin/guile \\\n"
  "-e main -s\n"
  "!#\n\n"
  @ _ ?\n?\n
  > "(define (main args)\n"
  > @ - ")\n")
  
(defalias 'gs 'guile-scheme)

(define-skeleton emacs-batch-script
  "Insert skeleton code for an Emacs batch script to be run from
 the command line."
  nil
  ":;exec emacs --no-site-file --batch \"$@\" -l \"$0\" -f main\n"
  @ ?\n
  > "(defun main ()\n"
  > @ - ")\n"
  '(progn
     (save-buffer)
     (set-file-modes (buffer-file-name) #o755)))

(defalias 'ebs 'emacs-batch-script)

(define-skeleton sbcl-cli-script
  "Insert skeleton code for a SBCL command line script."
  nil
  "#!/usr/bin/sbcl --script\n"
  ";; -*- Mode: lisp -*-\n\n"
  > "(load (merge-pathnames \".sbclrc\" (user-homedir-pathname)))\n"
  > "(ql:quickload '(:unix-options) :silent t)\n\n"
  > "(use-package :unix-options)\n\n")

(define-skeleton sbcl-pg-script
  "Insert skeleton for a SBCL command line script that uses cl-pg."
  nil
  "#!/usr/bin/sbcl --script\n"
  ";; -*- Mode: lisp -*-\n\n"
  > "(load (merge-pathnames \".sbclrc\" (user-homedir-pathname)))\n"
  > "(ql:quickload (:unix-options :pg) :silent t)\n"
  > "(use-package :unix-options)\n(use-package :pg)\n\n"
  > "(with-cli-options ()\n"
  > "(&parameters username password host dbname port)\n"
  > "(with-pg-connection (conn dbname username :host host :port (parse-integer port) :password password)\n))")

(defvar sob-name-history nil "History list of signed-off-by names.")
(defvar sob-email-history nil "History list of signed-off-by emails.")

(define-skeleton signed-off-by
  "Prompts for name and email to add a signed-off-by."
  nil
  "Signed-off-by: " (read-string "Name? " nil 'sob-name-history user-full-name)
  " <" (read-string "Email address? " nil 'sob-email-history user-mail-address) ">")

(defalias 'sob 'signed-off-by)

(defvar cab-name-history nil "History list of co-authored-by names.")
(defvar cab-email-history nil "History list of co-authored-by emails.")

(define-skeleton co-authored-by
  "Prompts for name and email to add a co-authored-by."
  nil
  "Co-authored-by: " (read-string "Name? " nil 'cab-name-history)
  " <" (read-string "Email address? " nil 'cab-email-history) ">")

(defalias 'cab 'co-authored-by)

(define-skeleton egdbi
  "Inserts a typical set of code to start a Perl DBI script for Evergreen."
  nil
  > "#!/usr/bin/perl\n\n"
  > "use strict;\n"
  > "use warnings;\n"
  > "use Getopt::Long qw(:config no_ignore_case no_auto_abbrev);\n"
  > "use DBI;\n"
  > "use DBD::Pg qw(:pg_types);\n\n"
  > @ _ ?\n?\n
  > "# DBI options with defaults:\n"
  > "my $db_user = $ENV{PGUSER} || 'evergreen';\n"
  > "my $db_host = $ENV{PGHOST} || 'db2';\n"
  > "my $db_database = $ENV{PGDATABASE} || 'evergreen';\n"
  > "my $db_password = $ENV{PGPASSWORD} || 'evergreen';\n"
  > "my $db_port = $ENV{PGPORT} || 5432;\n\n"
  > "GetOptions('user|U=s' => \\$db_user,\n"
  > "'host|h=s' => \\$db_host,\n"
  > "'database|d=s' => \\$db_database,\n"
  > "'password|P=s' => \\$db_password,\n"
  > "'port|p=i' => \\$db_port)\n"
  > "or die('Error in command line arguments');\n\n"
  > "my $dbh = DBI->connect(\"dbi:Pg:database=$db_database;host=$db_host;port=$db_port\",\n"
  > "$db_user, $db_password,{AutoCommit=>1}) or die('No database connection');\n\n"
  @ - ?\n
  > "$dbh->disconnect();")

(define-skeleton egdbu
  "Inserts boiler plate for an Evergreen database upgrade script."
  nil
  @ "BEGIN;\n\n--SELECT evergreen.upgrade_deps_block_check('XXXX', :eg_version);\n\n"
  @ _ ?\n?\n
    "COMMIT;\n")

(define-skeleton egmarcxml
  "Inserts use statements for using MARC with the Evergreen
database.  If run with a prefix argument, it also wraps the egdbi
skeleton output around the use statements."
  nil
  > "use MARC::Record;\n"
  > "use MARC::File::XML (BinaryEncoding => 'utf8');\n"
  > "use OpenILS::Utils::Normalize qw(clean_marc);"
  '(if current-prefix-arg
       (let ((current-prefix-arg nil))
         (mark-whole-buffer)
         (egdbi))))

(define-skeleton autofitxlsx
  "Inserts autofit code for the Excel::Writer::XLSX Perl module."
  nil
  @ > "###############################################################################\n"
  > "# Functions used for Autofit.\n"
  > "# Stolen from the Spreadsheet::WriteExcel examples:\n"
  > "# https://metacpan.org/pod/Spreadsheet::WriteExcel::Examples#Example:-autofit.pl\n"
  > "###############################################################################\n\n"
  > "###############################################################################\n"
  > "# Adjust the column widths to fit the longest string in the column.\n"
  > "#\n"
  > "# Call this on each worksheet like so:\n"
  > "# autofit_columns($worksheet);\n"
  > "#\n"
  > "sub autofit_columns {\n"
  > "my $worksheet = shift;\n"
  > "my $col = 0;\n\n"
  > "for my $width (@{$worksheet->{__col_widths}}) {\n"
  > "$worksheet->set_column($col, $col, $width) if $width;\n"
  > "$col++;\n"
  > "}\n}\n\n"
  > "###############################################################################\n"
  > "# The following function is a callback that was added via add_write_handler()\n"
  > "# above. It modifies the write() function so that it stores the maximum\n"
  > "# unwrapped width of a string in a column.\n"
  > "#\n"
  > "# Add this as a write_handler to each worksheet that you want to autofit:\n"
  > "# $worksheet->add_write_handler(qr[\\w], \\&store_string_widths);\n"
  > "#\n"
  > "sub store_string_widths {\n"
  > "my $worksheet = shift;\n"
  > "my $col = $_[1];\n"
  > "my $token = $_[2];\n\n"
  > "# Ignore some tokens that we aren't interested in.\n"
  > "return if not defined $token;       # Ignore undefs.\n"
  > "return if $token eq '';             # Ignore blank cells.\n"
  > "return if ref $token eq 'ARRAY';    # Ignore array refs.\n"
  > "return if $token =~ /^=/;           # Ignore formula\n"
  > "# Ignore numbers\n"
  > "return if $token =~ /^([+-]?)(?=\\d|\\.\\d)\\d*(\\.\\d*)?([Ee]([+-]?\\d+))?$/;\n"
  > "# Ignore various internal and external hyperlinks. In a real scenario\n"
  > "# you may wish to track the length of the optional strings used with\n"
  > "# urls.\n"
  > "return if $token =~ m{^[fh]tt?ps?://};\n"
  > "return if $token =~ m{^mailto:};\n"
  > "return if $token =~ m{^(?:in|ex)ternal:};\n\n"
  > "# We store the string width as data in the Worksheet object. We use\n"
  > "# a double underscore key name to avoid conflicts with future names.\n"
  > "my $old_width = $worksheet->{__col_widths}->[$col];\n"
  > "my $string_width = 0;\n"
  > "# Handle multi-line fields by splitting token on newline:\n"
  > "my @toks = split(/\\n/, $token);\n"
  > "for my $tok (@toks) {\n"
  > "my $width = length($tok);\n"
  > "$string_width = $width if ($width > $string_width);\n"
  > "}\n\n"
  > "if (not defined $old_width or $string_width > $old_width) {\n"
  > "# You may wish to set a minimum column width as follows.\n"
  > "#return undef if $string_width < 10;\n"
  > "$worksheet->{__col_widths}->[$col] = $string_width;\n"
  > "}\n"
  > "# Return control to write();\n"
  > "return undef;\n"
  > "}\n" @)

(define-skeleton yaous-upsert
  "Create upsert block for actor.org_unit_setting."
  nil
  '(setq str (skeleton-read "Name of setting: ")
         v1 (skeleton-read "Org Unit ID: " "1")
         v2 (skeleton-read "Setting value: "))
  "INSERT INTO actor.org_unit_setting\n"
  "(org_unit, name, value)\n"
  "VALUES (" v1 ", '" str "', '" v2 "')\n"
  "ON CONFLICT ON CONSTRAINT ou_once_per_key\n"
  "DO UPDATE\n"
  "SET value = '" v2 "'\n"
  "WHERE org_unit_setting.org_unit = " v1 ?\n
  "AND org_unit_setting.name = '" str "';\n")

(define-skeleton pgdo
  "Create a skeleton for a PostgreSQL DO block."
  nil
  "DO\n"
  "$$\n"
  "DECLARE\n"
  @ _ | ("Enter variable name: " "    " str
  " " (skeleton-read "Enter variable type: ") ";\n")
  "BEGIN\n"
  @ _ | ?\n
  "END\n"
  "$$;")

(define-skeleton evergreen-psycopg2
  "Create a skeleton for a Python script to talk to Evergreen."
  nil
  "#!/usr/bin/env python3\n\n"
  "import argparse, psycopg2\n"
  @ _ | ?\n
  "parser = argparse.ArgumentParser()\n"
  "parser.add_argument('--user', default='evergreen')\n"
  "parser.add_argument('--host', default='localhost')\n"
  "parser.add_argument('--database', default='evergreen')\n"
  "parser.add_argument('--port', default=35432, type=int)\n"
  @ _ | ?\n
  "args = parser.parse_args()\n"
  @ _  | ?\n
  "with psycopg2.connect(host=args.host, port=args.port, user=args.user, dbname=args.database) as conn:\n"
  > "with conn.cursor() as cursor:\n"
  @ _)

(define-skeleton egcronscript
  "Insert a basic skeleton for a Cronscript.pm-based Perl script."
  nil
  "#!/usr/bin/perl\n"
  "use strict;\n"
  "use warnings;\n"
  "use OpenILS::Utils::Cronscript;\n"
  @ _ ?\n
  "my $U = 'OpenILS::Application::AppUtils';\n\n"
  "my $script = OpenILS::Utils::Cronscript->new({nolockfile=>1});\n"
  @ _)

(define-skeleton egcronscript-auth
  "Insert a skeleton for a Conscript.pm-based Perl script that
 requires authentication."
  nil
  "#!/usr/bin/perl\n"
  "use strict;\n"
  "use warnings;\n"
  "use OpenILS::Utils::Cronscript;\n"
  @ _ ?\n
  "my $U = 'OpenILS::Application::AppUtils';\n\n"
  "my %defaults = (\n"
  > "'username=s' => '',\n"
  > "'password=s' => '',\n"
  > "'workstation=s' => '',\n"
  > "'staff' => 1,\n"
  > "nolockfile => 1\n"
  ");\n\n"
  "my $script = OpenILS::Utils::Cronscript->new(\\%defaults);\n"
  "my $opts = $script->MyGetOptions();\n\n"
  "my $authtoken = $script->authenticate({\n"
  > "username => $opts->{username},\n"
  > "password => $opts->{password},\n"
  > "workstation => $opts->{workstation},\n"
  > "staff => $opts->{staff}\n"
  "});\n\n"
  "END {\n"
  "$script->logout();\n"
  > "}\n"
  @ _)

(define-skeleton egperl-live-test
  "Insert a skeleton for an Evergreen Perl live test script."
  "Test name: "
  > "#!perl\n"
  > "use strict; use warnings;\n"
  > "use Test::More;\n"
  > "use OpenILS::Utils::TestUtils;\n"
  > "use OpenILS::Const qw(:const);\n"
  ?\n
  > "my $script = OpenILS::Utils::TestUtils->new();\n"
  > "my $U = 'OpenILS::Application::AppUtils';\n"
  ?\n
  "diag('" str "');\n"
  ?\n
  "done_testing();\n")

(define-skeleton text-csv-headers
  "Insert Perl code to read headers from first line of a csv using Text::CSV."
  "Filehandle: "
  > "my @cols = @{$csv->getline(" str ")};\n"
  > "my $row = {};\n"
  > "$csv->bind_columns(\\@{$row}{@cols});\n")

(define-skeleton text-csv-readloop
  "Insert Perl code to read from a CSV opened with Text::CSV."
  "Filehandle: "
  > "while ($csv->getline(" str ")) {\n"
  @ _ ?\n
  > "}\n")

(define-skeleton pqxx+boost
  "Insert a skeleton for a C++ program using libpqxx and boost to
 connect to PostgreSQL for Evergreen."
  ""
  > "#include <boost/program_options.hpp>\n"
  > "#include <boost/optional.hpp>\n"
  > "#include <pqxx/pqxx>\n"
  > "#include <iostream>\n"
  > "#include <sstream>\n"
  @ _ ?\n
  > "namespace po = boost::program_options;\n"
  ?\n
  > "// Function declarations that we implement below main:\n"
  > "std::string map_environment(std::string env_var);\n"
  > "std::string build_connection_string();\n"
  ?\n
  > "// Global variables for database connection:\n"
  > "std::string username;\n"
  > "boost::optional<std::string> password;\n"
  > "std::string database;\n"
  > "std::string hostname;\n"
  > "unsigned short port;\n"
  @ ?\n
  > "auto main(int argc, char *argv[]) -> int {\n"
  ?\n
  > "// Program options for the database connection:\n"
  > "po::options_description dbopts(\"Database Options\");\n"
  > "dbopts.add_options()\n"
  > "(\"username,U\", po::value<std::string>(&username)->default_value(\""
  (skeleton-read "Enter default username: ") "\"), \"PostgreSQL Username\")\n"
  > "(\"password,P\", po::value(&password), \"PostgreSQL Password\")\n"
  > "(\"database,d\", po::value<std::string>(&database)->default_value(\""
  (skeleton-read "Enter default database: ") "\"), \"PostgreSQL Database\")\n"
  > "(\"host,h\", po::value<std::string>(&hostname)->default_value(\""
  (skeleton-read "Enter default host: ") "\"), \"PostgreSQL Host\")\n"
  > "(\"port,p\", po::value<unsigned short>(&port)->default_value("
  (skeleton-read "Enter default port: ") "), \"PostgreSQL Port\");\n"
  ?\n
  > "po::options_description all;\n"
  > "all.add(dbopts);\n"
  ?\n
  > "po::variables_map vm;\n"
  > "try {\n"
  > "po::store(po::command_line_parser(argc, argv).options(all).run(), vm);\n"
  > "po::store(po::parse_environment(all, boost::function1<std::string, std::string>(map_environment)), vm);\n"
  > "po::notify(vm);\n"
  > "} catch (po::multiple_occurrences &mo) {\n"
  > "std::cerr << mo.what() << \": \" << mo.get_option_name() << \"\\n\";\n"
  > "std::exit(EXIT_FAILURE);\n"
  > "} catch (std::exception &e) {\n"
  > "std::cerr << \"An exception occurred parsing arguments: \";\n"
  > "std::cerr << e.what() << \"\\n\";\n"
  > "std::cerr << all << \"\\n\";\n"
  > "std::exit(EXIT_FAILURE);\n"
  > "}\n"
  ?\n
  > "try {\n"
  > "pqxx::connection dbconnection(build_connection_string());\n"
  > "if (dbconnection.is_open()) {\n"
  @ _ | ?\n
  > "dbconnection.disconnect();\n"
  > "}\n"
  > "} catch (std::exception &e) {\n"
  > "std::cerr << e.what() << \"\\n\";\n"
  > "std::exit(EXIT_FAILURE);\n"
  > "}\n"
  ?\n
  > "return 0;\n"
  > "}\n"
  ?\n
  > "// Maps envivornment variables to command line options, so we can read\n"
  > "// missing options from the environment.\n"
  > "std::string map_environment(std::string env_var) {\n"
  > "if (env_var == \"PGDATABASE\") return \"database\";\n"
  > "if (env_var == \"PGUSER\") return \"username\";\n"
  > "if (env_var == \"PGHOST\") return \"host\";\n"
  > "if (env_var == \"PGPORT\") return \"port\";\n"
  > "if (env_var == \"PGPASSWORD\") return \"password\";\n"
  > "return \"\";\n"
  > "}\n"
  ?\n
  > "// Build the database connection string\n"
  > "std::string build_connection_string() {\n"
  > "std::ostringstream connection_spec {};\n"
  > "connection_spec << \"dbname=\" << database\n"
  > "<< \" user=\" << username\n"
  > "<< \" host=\" << hostname\n"
  > "<< \" port=\" << port;\n"
  > "if (password)\n"
  > "connection_spec << \" password=\" << password.value();\n"
  > "return connection_spec.str();\n"
  > "}\n")

(define-skeleton pqxx+getopt
  "Skeleton for a C++ main program to connect to PostgreSQL using
 getopt_long and getenv."
  ""
  "#include <pqxx/pqxx>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <optional>
#include <getopt.h>
"
  @ _ ?\n
  "// Functions defined after main():
std::optional<std::string> maybe_getenv(const char *n);
std::optional<unsigned short> maybe_getport();
std::string build_connection_string();
unsigned short stous(const std::string& s);

// Global variables for database connection:
std::optional<std::string> username = maybe_getenv(\"PGUSER\");
std::optional<std::string> password = maybe_getenv(\"PGPASSWORD\");
std::optional<std::string> database = maybe_getenv(\"PGDATABASE\");
std::optional<std::string> hostname = maybe_getenv(\"PGHOST\");
std::optional<unsigned short> port = maybe_getport();
"
  @ ?\n
"auto main(int argc, char **argv) -> int {

  struct option longoptions[] = {
    {\"username\", required_argument, 0, 'U'},
    {\"password\", required_argument, 0, 'P'},
    {\"database\", required_argument, 0, 'd'},
    {\"host\", required_argument, 0, 'h'},
    {\"port\", required_argument, 0, 'p'},
    {0, 0, 0, 0}
  };

  int ch;
  while ((ch = getopt_long(argc, argv, \"U:P:d:h:p:\", longoptions, 0)) != -1) {
    switch (ch) {
    case 'd':
      database.emplace(optarg);
      break;
    case 'h':
      hostname.emplace(optarg);
      break;
    case 'p':
      port.emplace(stous(optarg));
      break;
    case 'P':
      password.emplace(optarg);
      break;
    case 'U':
      username.emplace(optarg);
      break;
    default:
      std::exit(EXIT_FAILURE);
    }
  }

  try {
    pqxx::connection dbconnection(build_connection_string());
    if (dbconnection.is_open()) {
"
  @ _ | ?\n
"      dbconnection.disconnect();
    }
  } catch (std::exception &e) {
    std::cerr << e.what() << \"\\n\";
    std::exit(EXIT_FAILURE);
  }

  return 0;
}

// Get an optional string value from the environment.
std::optional<std::string> maybe_getenv(const char *n) {
  if (const char *x = getenv(n)) return std::string(x);
  return std::nullopt;
}

// Get the value of PGPORT from the environment if it is set.
std::optional<unsigned short> maybe_getport() {
  std::optional<std::string> port = maybe_getenv(\"PGPORT\");
  if (port) return stous(port.value());
  return std::nullopt;
}

// Build the database connection string
std::string build_connection_string() {
  std::ostringstream connection_spec {};
  connection_spec << \"dbname=\" << database.value_or(\"" (skeleton-read "Default database: ") "\")
                  << \" user=\" << username.value_or(\"" (skeleton-read "Default username: ") "\")
                  << \" host=\" << hostname.value_or(\"" (skeleton-read "Default hostname: ") "\")
                  << \" port=\" << port.value_or(" (skeleton-read "Default port: ") ");
  if (password)
    connection_spec << \" password=\" << password.value();
  return connection_spec.str();
}

// Convert string to unsigned short and throw exception if value is
// out of range.
unsigned short stous(const std::string& s) {
  int i = std::stoi(s);
  if (i < 0 || i > 0xFFFF) {
    std::ostringstream osstr {};
    osstr << \"stous: \" << i << \" is out of range for unsigned short.\";
    throw std::out_of_range(osstr.str());
  }
  return static_cast<unsigned short>(i);
}\n")

(define-skeleton personal-html-page
  "Skeleton for a web page under my personal public_html directory."
  "Page Title: "
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<title>" str "</title>
<link rel=\"stylesheet\" type=\"text/css\" href=\"navajo.css\" title=\"Navajo\" media=\"all\">
</head>
<body>\n\n"
    _
  "\n</body>
</html>")

(define-skeleton sigio-html-page
  "Skeleton to create a web page for my Sigio site."
  "Page Title: "
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<title>" str "</title>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/screen.css\" media=\"all\"
      title=\"Site Default\">
<link rel=\"alternate stylesheet\" type=\"text/css\" href=\"/styles/article.css\"
      media=\"all\" title=\"Article\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/print.css\" media=\"print\">
</head>
<body>\n\n"
_
"\n</body>
</html>")

(define-skeleton evergreen-html-page
  "Skeleton for a page in my Evergreen services section."
  "Page Title: "
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<title>" str "</title>
<link rel=\"alternate stylesheet\" type=\"text/css\" href=\"/styles/screen.css\"
      media=\"all\" title=\"Site Default\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/article.css\"
      media=\"all\" title=\"Article\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/print.css\" media=\"print\">
<style type=\"text/css\">
@import url(/styles/services.css);
</style>
</head>
<body>\n\n"
_
"\n</body>
</html>")

(define-skeleton plain-html-page
  "Skeleton for a plain HTML5 page."
  "Page Title: "
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<title>" str "</title>
</head>
<body>

<h1>" str "</h1>\n\n"
_
"\n</body>
</html>")

(define-skeleton readme-md
  "Skeleton for a github flavoerd Markdown README."
  "Project title: "
  "# " str  & ?\n
  @ _ | ?\n
  "## Description\n"
  @ _ | ?\n
  "## Getting Started

### Dependencies\n"
  @ _ | ?\n
    "### Installing\n"
    @ _ | ?\n
      "### Running the Program\n"
      @ _ ?\n
      "## Authors

[Jason Stephenson](https://www.sigio.com/) is responsible for most of
the code here.

## License\n\n"
      str " is licensed  under the terms of the " (skeleton-read "License: ")
      " - see [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments\n\n"
      @ _ | ?\n)

(provide 'my-skeletons)
