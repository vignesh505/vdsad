(* givenSafe Function *)
fun givenSafe (d, t, e) =
  case (d, t, e) of
    (13, 30, 30) => true
  | (6, 30, 10) => true
  | (27, 30, 50) => true
  | (13, 15, 50) => true
  | (13, 120, 10) => true
  | (27, 120, 30) => true
  | (6, 15, 30) => true
  | _ => false;

(* interpolatedSafe Function *)
fun interpolatedSafe (d, t, e) =
  if d >= 6 andalso d <= 27 andalso t >= 15 andalso t <= 120 andalso e >= 10 andalso e <= 50
  then true
  else false;

(* SAFETY_TABLE *)
val SAFETY_TABLE = 
  [(13,30,30),(6,30,10),(27,30,50),(13,15,50),(13,120,10),(27,120,30),(6,15,30)] : (int * int * int) list;

(* listDerivedSafe Function *)
fun listDerivedSafe (d, t, e) =
  let
    fun search [] = false
      | search ((d', t', e')::rest) =
        if (d = d') andalso (t = t') andalso (e = e')
        then true
        else search rest
  in
    search SAFETY_TABLE
  end;

(* printSafety Function *)
fun printSafety (safetyFn, (d, t, e)) =
  let
    val isSafe = safetyFn (d, t, e)
  in
    print ("Distance:" ^ Int.toString(d) ^ " Duration:" ^ Int.toString(t) ^
           " Exhalation:" ^ Int.toString(e) ^ " Safe:" ^ Bool.toString(isSafe) ^ "\n")
  end;

(* concisePrintSafety Function *)
fun concisePrintSafety (safetyFn, (d, t, e)) =
  let
    val isSafe = safetyFn (d, t, e)
  in
    print ("(" ^ Int.toString(d) ^ ", " ^ Int.toString(t) ^ ", " ^ Int.toString(e) ^
           ", " ^ Bool.toString(isSafe) ^ ")\n")
  end;

(* listPrintSafety Function *)
fun listPrintSafety (printFn, safetyFn, features) =
  let
    fun printAll [] = ()
      | printAll (f::fs) =
        (printFn (safetyFn, f);
        printAll fs)
  in
    printAll features
  end;

(* givenSafeMatcher and derivedSafeMatcher Functions *)
fun givenSafeMatcher ((d1, t1, e1), (d2, t2, e2)) =
  (d1 = d2) andalso (t1 = t2) andalso (e1 = e2);

fun derivedSafeMatcher ((d1, t1, e1), (d2, t2, e2)) =
  (abs (d1 - d2) <= 1) andalso (abs (t1 - t2) <= 1) andalso (abs (e1 - e2) <= 1);

(* matchingSafe Function *)
fun matchingSafe (matcherFn, feature) =
  List.exists (fn tableEntry => matcherFn (feature, tableEntry)) SAFETY_TABLE;

(* matchingDerivedSafe and matchingGivenSafe Functions *)
val matchingDerivedSafe = fn (d, t, e) => matchingSafe(derivedSafeMatcher, (d, t, e));
val matchingGivenSafe = fn (d, t, e) => matchingSafe(givenSafeMatcher, (d, t, e));

(* curryableInterpolatedSafe and Curried Functions *)
fun curryableInterpolatedSafe d t e = interpolatedSafe (d, t, e);
val curriedOnceInterpolatedSafe = curryableInterpolatedSafe 13;
val curriedTwiceInterpolatedSafe = curriedOnceInterpolatedSafe 30;

(* curryableMatchingSafe and Curried Functions *)
fun curryableMatchingSafe matcher feature = matchingSafe(matcher, feature);
val curriedMatchingDerivedSafe = curryableMatchingSafe derivedSafeMatcher;
val curriedMatchingGivenSafe = curryableMatchingSafe givenSafeMatcher;
