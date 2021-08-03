package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type PartType int

const (
	TICKS = iota
	VAL
	WHERE
	ALIASPART
	NONE
)

type DefKind int

const (
	INPUT = iota
	OUTPUT
	ALIAS
)

type TypeId struct {
	id  string
	typ string
}

type TypeAndDef struct {
	id          string
	typ         string
	defs        []string
	params      []TypeId
	constraints string
	hidden      bool
}

type HeaderInfo struct {
	constants            string
	libname              string
	innerspecname        string
	innerspectyp         string
	innerspecconstraints string
	innerspecparams      []TypeId
	retstream            string
	stopstream           string
	format               string
	imports              string
	verbatim             string
}

type Alias struct {
	id          string
	typ         string
	def         string
	params      []TypeId
	constraints string
	hidden      bool
}

type IOid struct {
	id      string
	defKind DefKind
}

var LINENUM int = 0

var outwriter *bufio.Writer
var outpath string = ""
var outfname string = "/Main.hs"

var typedInputs map[string]TypeAndDef = make(map[string]TypeAndDef)
var typedOutputs map[string]TypeAndDef = make(map[string]TypeAndDef)
var typedAlias map[string]Alias = make(map[string]Alias)
var lastid string = ""
var lastpart PartType = NONE
var order []IOid = make([]IOid, 0)
var headerinfo = new(HeaderInfo)

var inputTemplate string = `%s :: %s%sStream %s
%s = input "%s"
`
var inputIndentedTemplate string = `  %s :: %s%sStream %s
  %s = input "%s"
`

var aliasTemplate string = `
%s :: %s%sStream %s
%s %s= "%s" %s==: (%s)
`

var outputTemplate string = `
%s :: %s%sStream %s
%s %s= (let
  ticks =%s
  val = %s
  in "%s" %s=: (ticks,val)
  )%s
`
var outputIndentedTemplate string = `
  %s :: %s%sStream %s
  %s %s= (let
    ticks =%s
    val = %s
    in "%s" %s=: (ticks,val)
    )%s
`

/*
First argument: custom imports
Second argument: JSON/CSV
Third argument: custom haskell
*/
var headerMain = `{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import GHC.Generics
import Data.Aeson
import InFromFile
import System.IO
import System.Environment
import HStriver
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import Declaration.StaticAnalysis
import qualified Prelude as P
%s

data ExecMode = ExecSpec String String String | ShowHelp | Analyse

main :: IO ()
main = parseArgs P.<$> getArgs >>= runInMode

parseArgs :: [String] -> ExecMode
parseArgs ["--execute", dir, tsfield, valfield] = ExecSpec dir tsfield valfield
parseArgs ["analyse"] = Analyse
parseArgs _ = ShowHelp

runInMode :: ExecMode -> IO ()
runInMode (ExecSpec dir tsfield valfield) = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runSpecJSON (sysTimeGetter tsfield) (Files dir valfield) specification
runInMode Analyse = analyse specification
runInMode ShowHelp = putStr$unlines [
    "Wrong arguments. Usage:"
  , "  HStriver dir tsField valueField"
  , "Where dir indicates the directory to look for the input JSONs, and tsField and valueField are the fields where the timestamp and value of the events are placed."]

-- Custom Haskell
%s

-- Constants
%s
-- End of custom Haskell
`

var headerInner = `{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module INNERSPECSDIR.INNERSPEC_%s where
import GHC.Generics
import Data.Aeson
import Declaration.DecDyn (InnerSpecification(IS), bind)
import InFromFile
import System.IO
import System.Environment
import HStriver
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
%s

-- Custom Haskell
%s

%s :: %s %s %s InnerSpecification %s
%s %s %s = IS [%s] %s %s
  where
%s
`
/*
args: Libname, custom imports, custom haskell
*/
var headerLib = `{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib.%s where
import GHC.Generics
import Data.Aeson
import HStriver
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
%s

-- Custom Haskell
%s
`

// As seen on StackOverflow
func FindStringSubmatchMap(r *regexp.Regexp, s string) map[string]string {
	captures := make(map[string]string)
	match := r.FindStringSubmatch(s)
	if match == nil {
		return captures
	}
	for i, name := range r.SubexpNames() {
		// Ignore the whole regexp match and unnamed groups
		if i == 0 || name == "" {
			continue
		}
		captures[name] = match[i]
	}
	return captures
}

func findBalanced(s string, adder rune, remover rune) (string, string) {
	typ := new(bytes.Buffer)
	rest := new(bytes.Buffer)
	reader := strings.NewReader(s)
	balancecount := 1
	ch, _, err := reader.ReadRune()
	if err != nil || ch != adder {
		panic("Balanced error")
	}
	typ.WriteRune(ch)
	for balancecount > 0 {
		ch, _, err := reader.ReadRune()
		if err != nil {
			panic("Balanced error")
		}
		switch ch {
		case adder:
			balancecount++
		case remover:
			balancecount--
		}
		typ.WriteRune(ch)
	}
	rest.ReadFrom(reader)
	return typ.String(), rest.String()
}

func munchType(s string) (string, string) {
	s = strings.TrimSpace(s)
	switch s[0] {
	case '(':
		return findBalanced(s, '(', ')')
	case '[':
		return findBalanced(s, '[', ']')
	default:
		strs := strings.SplitN(s, " ", 2)
		return strs[0], strs[1]
	}
}

func parseError(s string) {
	panic(s)
}

func getLastWord(s string) (string, string) {
	s = strings.Trim(s, " ")
	lastwordRE := regexp.MustCompile(`^(?P<rest>.*)? (?P<lastword>[^ ]+)$`)
	themap := FindStringSubmatchMap(lastwordRE, s)
	if len(themap) < 2 {
		parseError("No stream name in line " + strconv.Itoa(LINENUM))
	}
	// typ := themap["type"]
	return themap["lastword"], strings.Trim(themap["rest"], " ")
}

func processConstDeclaration(s string) {
	if !strings.HasPrefix(s, "const ") {
		return
	}
	headerinfo.constants += getIndentation() + s[6:] + "\n"
}

func processAliasDeclaration(s string) {
	re := regexp.MustCompile(`^(?P<kind>output|define) (?P<lhs>([^=]|=>)*)=(?P<rhs>([^>].*$|$))`)
	themap := FindStringSubmatchMap(re, s)
	if len(themap) < 3 {
		return
	}
	hidden := themap["kind"] == "define"
	s = themap["lhs"]
	body := themap["rhs"]
	strs := strings.SplitN(s, "<", 2)
	s = strs[0]
	id, typConstr := getLastWord(s)
	typ := typConstr
	constr := ""
	if strings.Contains(typConstr, "=>") {
		strs := strings.SplitN(typConstr, "=>", 2)
		typ = strings.TrimSpace(strs[1])
		constr = strs[0] + "=> "
	}
	// rest = strs[1]
	params := []TypeId{}
	if len(strs) > 1 {
		rawParams := strings.Trim(strs[1], " ")
		rawParams = rawParams[:len(rawParams)-1]
		re := regexp.MustCompile("> *<")
		rawParams = re.ReplaceAllString(rawParams, "><")
		pars := strings.Split(rawParams, "><")
		for _, x := range pars {
			parid, typ := getLastWord(x)
			params = append(params, TypeId{parid, typ})
		}
	}
	typedAlias[id] = Alias{id, typ, body, params,
		constr, hidden}
	lastid = id
	lastpart = ALIASPART
	order = append(order, IOid{id, ALIAS})
}

func processOutputDeclaration(s string) {
	re := regexp.MustCompile(`^(?P<kind>output|define) (?P<lhs>([^:])*): *$`)
	themap := FindStringSubmatchMap(re, s)
	if len(themap) < 2 {
		return
	}
	hidden := themap["kind"] == "define"
	s = themap["lhs"]
	strs := strings.SplitN(s, "<", 2)
	s = strs[0]
	id, typConstr := getLastWord(s)
	typ := typConstr
	constr := ""
	if strings.Contains(typConstr, "=>") {
		strs := strings.SplitN(typConstr, "=>", 2)
		typ = strings.TrimSpace(strs[1])
		constr = strs[0] + "=> "
	}
	// rest = strs[1]
	params := []TypeId{}
	if len(strs) > 1 {
		rawParams := strings.Trim(strs[1], " ")
		rawParams = rawParams[:len(rawParams)-1]
		re := regexp.MustCompile("> *<")
		rawParams = re.ReplaceAllString(rawParams, "><")
		pars := strings.Split(rawParams, "><")
		for _, x := range pars {
			parid, typ := getLastWord(x)
			params = append(params, TypeId{parid, typ})
		}
	}
	typedOutputs[id] = TypeAndDef{id, typ, make([]string, 3), params,
		constr, hidden}
	lastid = id
	lastpart = NONE
	order = append(order, IOid{id, OUTPUT})
}

func getIdTypParamsConstraints(s string) (id string, typ string, params []TypeId, constr string) {
	strs := strings.SplitN(s, "<", 2)
	s = strs[0]
	id, typConstr := getLastWord(s)
	typ = typConstr
	constr = ""
	if strings.Contains(typConstr, "=>") {
		strs := strings.SplitN(typConstr, "=>", 2)
		typ = strings.TrimSpace(strs[1])
		constr = strs[0] + "=> "
	}
	// rest = strs[1]
	params = []TypeId{}
	if len(strs) > 1 {
		rawParams := strings.Trim(strs[1], " ")
		rawParams = rawParams[:len(rawParams)-1]
		re := regexp.MustCompile("> *<")
		rawParams = re.ReplaceAllString(rawParams, "><")
		pars := strings.Split(rawParams, "><")
		for _, x := range pars {
			parid, typ := getLastWord(x)
			params = append(params, TypeId{parid, typ})
		}
	}
	return
}

func processTickDeclaration(s string) {
	isTicks, err := regexp.MatchString("^  ticks *=", s)
	if err != nil {
		panic(err)
	}
	if !isTicks {
		return
	}
	lastpart = TICKS
	typedOutputs[lastid].defs[TICKS] = s[strings.Index(s, "=")+1:]
}
func processValDeclaration(s string) {
	isVal, err := regexp.MatchString("^  val *=", s)
	if err != nil {
		panic(err)
	}
	if !isVal {
		return
	}
	lastpart = VAL
	typedOutputs[lastid].defs[VAL] = s[strings.Index(s, "=")+1:]
}

func processWhereDeclaration(s string) {
	isWhere, err := regexp.MatchString("^  where( |$).*", s)
	if err != nil {
		panic(err)
	}
	if isWhere {
		lastpart = WHERE
		typedOutputs[lastid].defs[WHERE] = s
	}
}

func getParamTypes(params []TypeId) string {
	ret := ""
	for _, tyid := range params {
		ret = ret + tyid.typ + " -> "
	}
	return ret
}

func getParamNames(params []TypeId) string {
	ret := ""
	for _, tyid := range params {
		ret = ret + tyid.id + " "
	}
	return ret
}

func getParamIds(params []TypeId) string {
	ret := ""
	for _, tyid := range params {
		if !strings.Contains(tyid.typ, "->") {
			// Functions are not Show. Every other paramater is supposed to be an
			// instance of Show, and will be included
			ret = ret + "<: " + tyid.id + " "
		}
	}
	return ret
}

func processInputDeclaration(s string) {
	re := regexp.MustCompile(`^input (?P<lhs>.*)`)
	themap := FindStringSubmatchMap(re, s)
	if len(themap) == 0 {
		return
	}
	s = themap["lhs"]
	strs := strings.SplitN(s, "<", 2)
	s = strs[0]
	id, typConstr := getLastWord(s)
	typ := typConstr
	constr := ""
	if strings.Contains(typConstr, "=>") {
		strs := strings.SplitN(typConstr, "=>", 2)
		typ = strings.TrimSpace(strs[1])
		constr = strs[0] + "=> "
	}
	// rest = strs[1]
	params := []TypeId{}
	if len(strs) > 1 {
		rawParams := strings.Trim(strs[1], " ")
		rawParams = rawParams[:len(rawParams)-1]
		re := regexp.MustCompile("> *<")
		rawParams = re.ReplaceAllString(rawParams, "><")
		pars := strings.Split(rawParams, "><")
		for _, x := range pars {
			parid, typ := getLastWord(x)
			params = append(params, TypeId{parid, typ})
		}
	}
	typedInputs[id] = TypeAndDef{id, typ, nil, params,
		constr, false}
	lastid = id
	lastpart = NONE
	order = append(order, IOid{id, INPUT})
}

func processFileTypeDeclaration(s string) {
	libraryRE := regexp.MustCompile(`^library (?P<libname>.*)$`)
	themap := FindStringSubmatchMap(libraryRE, s)
	if len(themap) > 0 {
		if outwriter != nil && outpath != "" {
			panic("Library declared too late")
		}
		headerinfo.libname = themap["libname"]
		outfname = "/Lib/" + headerinfo.libname + ".hs"
	}
	innerspecRE := regexp.MustCompile(`^innerspec *(?P<rest>.*$)`)
	themap = FindStringSubmatchMap(innerspecRE, s)
	if len(themap) > 0 {
		if outwriter != nil && outpath != "" {
			panic("Innerspec declared too late")
		}
		s = themap["rest"]
		id, typ, params, constraints := getIdTypParamsConstraints(s)
		headerinfo.innerspecconstraints = constraints
		headerinfo.innerspecparams = params
		headerinfo.innerspecname = id
		headerinfo.innerspectyp = typ
		outfname = "/INNERSPECSDIR/INNERSPEC_" + headerinfo.innerspecname + ".hs"
	}
}

func processTimeDomainDeclaration(s string) {
	domainRE := regexp.MustCompile(`^time domain (?P<domain>(Double|UTC))$`)
	themap := FindStringSubmatchMap(domainRE, s)
	if len(themap) > 0 {
		useDomain(themap["domain"])
	}
}

func useDomain(domain string) {
	if outpath == "" {
		println("WARNING: defined time domain without path")
		return
	}
	timetfile := outpath + "/TimeDomain/TimeT.hs"
	domainfile := outpath + "/domains/Time" + domain
	if _, err := os.Stat(timetfile); err != nil {
		os.Rename(domainfile, timetfile)
	}
	if _, err := os.Stat(domainfile); err == nil {
		panic("Conflicting time domains")
	}
}

func processReturn(s string) {
	returnRE := regexp.MustCompile(`^return *(?P<retstream>.*)( *when *(?P<stopstream>.*)$)`)
	// TODO handle case with no when
	themap := FindStringSubmatchMap(returnRE, s)
	if len(themap) < 2 {
		if strings.HasPrefix(s, "return ") {
			panic("Ill return: " + s)
		}
		return
	}
	headerinfo.retstream = strings.Trim(themap["retstream"], " ")
	if val, ok := themap["stopstream"]; ok && val != "" {
		headerinfo.stopstream = strings.Trim(val, " ")
	} else {
		headerinfo.stopstream = "false__stream"
	}
}

func processUsage(s string) {
	useRE := regexp.MustCompile(`^use (?P<kind>(innerspec|library|theory|haskell)) (?P<name>.*)$`)
	themap := FindStringSubmatchMap(useRE, s)
	if len(themap) < 2 {
		if strings.HasPrefix(s, "use ") {
			panic("Unknown usage: " + s)
		}
		return
	}
	var usekind string
	switch themap["kind"] {
	case "library":
		usekind = "Lib." + themap["name"]
	case "theory":
		usekind = "Theories." + themap["name"]
	case "innerspec":
		usekind = "INNERSPECSDIR.INNERSPEC_" + themap["name"]
	default:
		usekind = "" + themap["name"]
	}
	newimport := "import " + usekind
	headerinfo.imports += newimport + "\n"
}

func liftFuns(s string) string {
	apix := strings.IndexByte(s, '\'')
	if apix == -1 {
		return s
	}
	//it's part of a name
	if apix > 0 {
		if isDigit(s[apix-1]) {
			if apix > 1 && idchar(s[apix-2]) {
				return s[:apix+1] + liftFuns(s[apix+1:])
			}
		} else if idchar(s[apix-1]) {
			return s[:apix+1] + liftFuns(s[apix+1:])
		}
	}
	// it's a char
	if apix+2 < len(s) && s[apix+2] == '\'' {
		return s[:apix+3] + liftFuns(s[apix+3:])
	}
	// we are lifting something:
	hasarity := false
	var arity byte
	upto := apix
	if apix > 0 && isDigit(s[apix-1]) {
		hasarity = true
		arity = s[apix-1]
		upto = apix - 1
	}
	lefthand := s[:upto]
	righthand := s[apix+1:]
	funame := ""
	rest := ""
	separator := " "
	if righthand[0] == '(' {
		funame, rest = findBalanced(righthand, '(', ')')
	} else {
		indexnl := strings.Index(righthand, "\n")
		indexspace := strings.Index(righthand, " ")
		if indexnl > 0 && indexnl < indexspace {
			separator = "\n"
		}
		splitt := strings.SplitN(righthand, separator, 2)
		funame = splitt[0]
		if len(splitt) == 2 {
			rest = splitt[1]
		}
	}
	if hasarity {
		righthand = "(magic" + string([]byte{arity}) + " " + funame + ")" + separator + rest
	} else {
		righthand = "(toolLift " + funame + ")" + separator + rest
	}
	return liftFuns(lefthand + righthand)
}

func isDigit(c byte) bool {
	return 47 < c && c < 58
}

func idchar(c byte) bool {
	return isDigit(c) || (c > 64 && c < 91) || c == 95 || (c > 96 && c < 123)
}

func printToFile(format string, args ...interface{}) {
	if outwriter == nil {
		if _, err := os.Stat(outpath + outfname); err == nil {
			fmt.Printf("WARNING: Duplicated file: " + outfname + "\n")
		}
		f, err := os.Create(outpath + outfname)
		if err != nil {
			panic(err)
		}
		outwriter = bufio.NewWriter(f)
	}
	fmt.Fprintf(outwriter, format, args...)
	outwriter.Flush()
}

func printIn(id string) {
	tad := typedInputs[id]
	template := inputTemplate
	if headerinfo.innerspecname != "" {
		template = inputIndentedTemplate
	}
	printToFile(template, id, tad.constraints, getParamTypes(tad.params), tad.typ, id, id)
}

func printAlias(id string) {
	tad := typedAlias[id]
	printToFile(aliasTemplate, id, tad.constraints, getParamTypes(tad.params), tad.typ, id, getParamNames(tad.params), id, getParamIds(tad.params), tad.def)
}

func replaceBoundedOffsets(def string) string {
	for strings.Contains(def, ":>>_") {
		arr := strings.SplitN(def, ":>>_", 2)
		prestr := arr[0]
		sufstr := arr[1]
		midstr := ""
		if sufstr[0] == '(' {
			midstr, sufstr = findBalanced(sufstr, '(', ')')
		} else {
			arr2 := strings.SplitN(sufstr, " ", 2)
			midstr = arr2[0]
			sufstr = arr2[1]
		}
		def = prestr + ">>| " + midstr + " `boundedApply` " + sufstr
	}
	return def
}

func replaceaccess(line string, restr, replacement string) string {
	findre := regexp.MustCompile(restr)
  replacere := regexp.MustCompile(restr[2:len(restr)-2])
	for loc := findre.FindStringIndex(line); loc != nil; loc = findre.FindStringIndex(line) {
		startix := loc[0]
		lefthand := line[:startix]
		righthand := line[startix:]
    midhand, righthand:= findBalanced(righthand, '[', ']')
    midhand = midhand[1:len(midhand)-1]
		replaced := replacere.ReplaceAllString(midhand, replacement)
		line = lefthand + replaced + righthand
}
	return line
}

func procVal(def string, splitby int) string {
	// Offset replacements:
	def = strings.Replace(def, ">>", ":>>", -1)
	def = strings.Replace(def, "~>", ":~>", -1)
	def = strings.Replace(def, "<<", ":<<", -1)
	def = strings.Replace(def, "<~", ":<~", -1)
	def = replaceBoundedOffsets(def)
	// Access RE:
	accessSuccDflt := regexp.MustCompile(`\[(?P<offset>[^~<][^|]*)>\|(?P<default>[^\]]+)\]`)
	accessSuccEqDflt := regexp.MustCompile(`\[(?P<offset>[^~<][^|]*)~\|(?P<default>[^\]]+)\]`)
	accessPrevDflt := regexp.MustCompile(`\[<(?P<offset>[^|]*[^~>])\|(?P<default>[^\]]+)\]`)
  // accessPrevEqDflt := regexp.MustCompile(`\[~(?P<offset>[^|]*[^~>])\|(?P<default>[^\]]+)\]`)
	accessSuccNoDflt := regexp.MustCompile(`\[(?P<offset>[^~<][^|]*)>\|(?P<qmark>\??)\]`)
	accessSuccEqNoDflt := regexp.MustCompile(`\[(?P<offset>[^~<][^|]*)~\|(?P<qmark>\??)\]`)
	accessPrevNoDflt := regexp.MustCompile(`\[<(?P<offset>[^|]*[^~>])\|(?P<qmark>\??)\]`)
	accessPrevEqNoDflt := regexp.MustCompile(`\[~(?P<offset>[^|]*[^~>])\|(?P<qmark>\??)\]`)
	cvRE := regexp.MustCompile(`\b(cv)\b`)
	// Access replacements:
	def = accessSuccNoDflt.ReplaceAllString(def, `@>$qmark($offset)`)
	def = accessSuccEqNoDflt.ReplaceAllString(def, `@~>$qmark($offset)`)
	def = accessPrevNoDflt.ReplaceAllString(def, `@<$qmark($offset)`)
	def = accessPrevEqNoDflt.ReplaceAllString(def, `@<~$qmark($offset)`)
	def = accessSuccDflt.ReplaceAllString(def, `@>($offset?|$default)`)
	def = accessSuccEqDflt.ReplaceAllString(def, `@~>($offset?|$default)`)
	def = accessPrevDflt.ReplaceAllString(def, `@<($offset?|$default)`)
	// def = accessPrevEqDflt.ReplaceAllString(def, `@<~($offset?|$default)`)
	def = replaceaccess(def, `\[~(?P<offset>[^|]*[^~>])\|(?P<default>.+)\]`, `@<~($offset?|$default)`)
	// cv replacement:
	if cvRE.MatchString(def) && splitby > 2 {
		panic("a U b U c U d not implemented")
	}
	def = cvRE.ReplaceAllString(def, `split`+strconv.Itoa(splitby)+`CV`)
	def = strings.Trim(def, " \t")
	// infty and out replacements:
	def = strings.Replace(def, "-infty", "NegInfty", -1)
	def = strings.Replace(def, "infty", "PosInfty", -1)
	def = strings.Replace(def, "-out", "NegOutside", -1)
	def = strings.Replace(def, "out", "PosOutside", -1)
	def = liftFuns(def)
	return def
}
func printOut(id string) {
	tad := typedOutputs[id]
	// Ticks:
	def := tad.defs[TICKS]
	singletonRE := regexp.MustCompile(`{(?P<singleton>[^}]*)}`)
	delayRENeg := regexp.MustCompile(`\b(delay-)`)
	delayREPos := regexp.MustCompile(`\b(delay\+)`)
	delayRE := regexp.MustCompile(`\b(delay )`)
	ticksOfRE := regexp.MustCompile(`\b(ticksOf )`)
	shiftRE := regexp.MustCompile(`\b(shift )`)
	// Replacements:
	def = delayRENeg.ReplaceAllString(def, `DelayTE Negative`)
	def = delayREPos.ReplaceAllString(def, `DelayTE Positive`)
	def = delayRE.ReplaceAllString(def, `DelayTE Positive `)
	def = singletonRE.ReplaceAllString(def, `ConstTE $singleton`)
	def = ticksOfRE.ReplaceAllString(def, `ticksTE `)
	def = shiftRE.ReplaceAllString(def, `ShiftTE `)
	splitby := strings.Count(def, ` U `)
	def = strings.Replace(def, ` U `, ` :+ `, -1)
	ticksdef := def
	if ticksdef == "" {
		panic("No ticks definition for " + id)
	}
	// Vals:
	def = tad.defs[VAL]
	def = procVal(def, splitby)
	valdef := def
	if valdef == "" {
		panic("No val definition for " + id)
	}
	// Where:
	def = tad.defs[WHERE]
	def = procVal(def, splitby)
	wheredef := def
	if wheredef != "" {
		wheredef = "\n" + getIndentation() + wheredef
	}
	template := outputTemplate
	if headerinfo.innerspecname != "" {
		template = outputIndentedTemplate
	}
	printToFile(template, id, tad.constraints, getParamTypes(tad.params), tad.typ, id, getParamNames(tad.params), ticksdef, valdef, id, getParamIds(tad.params), wheredef)
}

func process(s string) {
	if strings.HasPrefix(s, "  ") {
		if lastpart == ALIASPART {
			thedef := typedAlias[lastid]
      thedef.def += "\n" + getIndentation() + s
			typedAlias[lastid] = thedef
      return
		}
	}
	if strings.HasPrefix(s, "   ") {
		if lastid == "" || lastpart == NONE {
			panic("No last output or part")
		}
		thedef := typedOutputs[lastid]
    thedef.defs[lastpart] += "\n" + getIndentation() + s
		typedOutputs[lastid] = thedef
		return
	}
	processWhereDeclaration(s)
	processInputDeclaration(s)
	processOutputDeclaration(s)
	processAliasDeclaration(s)
	processTickDeclaration(s)
	processValDeclaration(s)
	processFileTypeDeclaration(s)
	processTimeDomainDeclaration(s)
	processUsage(s)
	processReturn(s)
	processConstDeclaration(s)
	//fmt.Println(s)
}

func getIndentation() string {
	if headerinfo.innerspecname != "" {
		return "  "
	}
	return ""
}

func isMainSpec() bool {
	return headerinfo.libname == "" && headerinfo.innerspecname == ""
}

func printSystem() {
	if !isMainSpec() {
		return
	}
	printToFile(`specification :: Specification
specification = [`)
	outputs := make([]string, 0, len(typedOutputs)+len(typedAlias))
	for id, tad := range typedOutputs {
		if len(tad.params) == 0 && !tad.hidden {
			outputs = append(outputs, id)
		}
	}
	for id, tad := range typedAlias {
		if len(tad.params) == 0 && !tad.hidden {
			outputs = append(outputs, id)
		}
	}
	fst := true
	for _, id := range outputs {
		if !fst {
			printToFile(", ")
		}
		fst = false
		printToFile("out " + id)
	}
	printToFile("]\n")
}

func printHeader() {
	if headerinfo.libname != "" {
		printToFile(headerLib, headerinfo.libname, headerinfo.imports, headerinfo.verbatim)
	} else if headerinfo.innerspecname != "" {
		var inputstreamtypes []string = make([]string, 0)
		var inputstreamargnames []string = make([]string, 0)
		var bindings []string = make([]string, 0)
		for _, idout := range order {
			if idout.defKind == INPUT {
				id := idout.id
				typ := typedInputs[id].typ
				inputstreamtypes = append(inputstreamtypes, "[(TimeT, "+typ+")] ->")
				argname := id + "__arg"
				inputstreamargnames = append(inputstreamargnames, argname)
				bindings = append(bindings, "bind "+id+" "+argname)
			}
		}

		var argtypes []string = make([]string, 0)
		var argnames []string = make([]string, 0)
		for _, tid := range headerinfo.innerspecparams {
			argtypes = append(argtypes, tid.typ+" ->")
			argnames = append(argnames, tid.id)
		}

		innername := headerinfo.innerspecname
		printToFile(headerInner, innername, headerinfo.imports, headerinfo.verbatim,
			innername,
			headerinfo.innerspecconstraints,
			strings.Join(argtypes, " "),
			strings.Join(inputstreamtypes, " "),
			headerinfo.innerspectyp,
			innername,
			strings.Join(argnames, " "),
			strings.Join(inputstreamargnames, " "),
			strings.Join(bindings, ", "),
			headerinfo.retstream,
			headerinfo.stopstream,
			headerinfo.constants)
	} else {
		printToFile(headerMain, headerinfo.imports, headerinfo.verbatim, headerinfo.constants)
	}
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	outwriter = bufio.NewWriter(os.Stdout)
	if len(os.Args) > 2 {
		outpath = os.Args[1]
		outwriter = nil
		infile, _ := os.Open(os.Args[2])
		defer infile.Close()
		scanner = bufio.NewScanner(infile)
	}
	verbatim := false
	for scanner.Scan() {
		LINENUM = LINENUM + 1
		txt := scanner.Text()
		if txt == "#ENDOFHASKELL" {
			verbatim = false
			continue
		}
		if txt == "#HASKELL" {
			verbatim = true
			continue
		}
		if verbatim {
			headerinfo.verbatim += txt + "\n"
			//fmt.Println(txt)
			continue
		}
		process(scanner.Text())
	}
	printHeader()
	printSystem()
	for _, idout := range order {
		switch idout.defKind {
		case INPUT:
			printIn(idout.id)
		case OUTPUT:
			printOut(idout.id)
		case ALIAS:
			printAlias(idout.id)
		}
	}
}
