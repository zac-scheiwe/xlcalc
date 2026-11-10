Attribute VB_Name = "VxMain"
Option Explicit

Public GL As VxLife
Public GSink As VxSink
Public GXl As VxXl

Public Sub Nop()
End Sub

Private Function Ex(ByVal tag As String) As String
    Ex = tag & " err " & Err.Number & " [" & Err.Description & "] src=" & Err.Source
End Function

Public Function Imp1() As String
    On Error GoTo EH
    Dim s As VxSq, sh As VxShape, nm As VxNamed
    Set s = New VxSq
    s.Side = 3
    s.Tag = "s"
    Set sh = s
    Set nm = s
    XLog.Lg "area " & sh.Area & " id " & sh.Id
    XLog.Lg "labels " & s.Label & " " & sh.Label & " " & nm.Label
    sh.Note = "hello"
    XLog.Lg "note " & s.Note
    sh.Draw
    sh.Draw 4
    sh.Draw times:=2
    XLog.Lg "typeof " & (TypeOf s Is VxShape) & (TypeOf s Is VxNamed) & (TypeOf sh Is VxSq) & (TypeOf nm Is VxShape) & (TypeOf sh Is VxNamed)
    XLog.Lg "names " & TypeName(s) & " " & TypeName(sh) & " " & TypeName(nm)
    XLog.Lg "is " & (sh Is nm) & (s Is sh) & (s Is nm)
    Dim o As Object
    Set o = nm
    Set sh = o
    XLog.Lg "via obj " & sh.Area & " " & o.Label
    Imp1 = "ok"
    Exit Function
EH:
    Imp1 = Ex("imp1")
End Function

Public Function Imp2() As String
    On Error Resume Next
    Dim st As VxStub, sh As VxShape, v As Variant, s As String
    Set st = New VxStub
    Set sh = st
    v = sh.Area
    s = "area:" & Err.Number & ":" & Err.Description & ":" & Err.Source & "|"
    Err.Clear
    v = sh.Id
    s = s & "id:" & Err.Number & ":" & Err.Description & ":" & Err.Source & "|"
    Err.Clear
    v = sh.Label
    s = s & "label:" & Err.Number & ":" & TypeName(v) & "|"
    Imp2 = s
End Function

Public Function Imp3() As String
    On Error Resume Next
    Dim o As Object, s As String, v As Variant
    Set o = New VxSq
    o.Side = 2
    v = o.Label
    s = "label " & v & " " & Err.Number & "|"
    Err.Clear
    v = o.Area
    s = s & "area " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    v = o.VxShape_Area
    s = s & "r7shape_area " & Err.Number & "|"
    Err.Clear
    v = CallByName(o, "Area", VbGet)
    s = s & "cbn area " & Err.Number & "|"
    Err.Clear
    v = CallByName(o, "Label", VbGet)
    s = s & "cbn label " & v & " " & Err.Number & "|"
    Err.Clear
    Dim sh As VxShape
    Set sh = o
    v = CallByName(sh, "Area", VbMethod)
    s = s & "cbn sh area " & v & " " & Err.Number & "|"
    Err.Clear
    Dim n As VxNamed
    Set n = o
    Set o = n
    v = o.Label
    s = s & "o via named " & v & " " & Err.Number & "|"
    Err.Clear
    Imp3 = s
End Function

Public Function Imp4() As String
    On Error GoTo EH
    Dim s As VxSq, sh As VxShape
    Set s = New VxSq
    s.Tag = "x"
    Set sh = s
    Set s = Nothing
    XLog.Lg "s gone, sh holds"
    Set sh = Nothing
    XLog.Lg "sh gone"
    Imp4 = "ok"
    Exit Function
EH:
    Imp4 = Ex("imp4")
End Function

Public Function Imp5() As String
    On Error Resume Next
    Dim n As VxNamed, o As Object, sh As VxShape, s As String
    Set o = New VxLife
    Set sh = o
    s = "life as shape " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Set o = New VxSq
    Set n = o
    Set sh = n
    s = s & "named obj to shape " & Err.Number & "|"
    Err.Clear
    Dim v As Variant
    Set v = New VxProp
    s = s & TypeName(v) & "|"
    Set o = Nothing
    Set sh = o
    s = s & "nothing " & (sh Is Nothing) & Err.Number & "|"
    Imp5 = s
End Function

Public Function Ev1() As String
    On Error GoTo EH
    Dim e As VxSrc, a As VxSink, b As VxSink
    Set e = New VxSrc
    XLog.Lg "none: " & e.Raise1(1)
    Set a = New VxSink: a.Tag = "A": Set a.Src = e
    XLog.Lg "one: " & e.Raise1(2)
    Set b = New VxSink: b.Tag = "B": Set b.Src = e
    XLog.Lg "two: " & e.Raise1(3)
    Set a.Src = Nothing
    XLog.Lg "a off: " & e.Raise1(4)
    Set a.Src = e
    XLog.Lg "a re-on: " & e.Raise1(5)
    Ev1 = "ok"
    Exit Function
EH:
    Ev1 = Ex("ev1")
End Function

Public Function Ev2() As String
    On Error GoTo EH
    Dim e As VxSrc, a As VxSink, b As VxSink, c As VxSink
    Set e = New VxSrc
    XLog.Lg "veto none " & e.RaiseVeto & " mod none " & e.RaiseMod("m")
    Set a = New VxSink: a.Tag = "A": Set a.Src = e
    Set b = New VxSink: b.Tag = "B": Set b.Src = e
    Set c = New VxSink: c.Tag = "C": Set c.Src = e
    XLog.Lg "veto " & e.RaiseVeto
    b.Veto = True
    XLog.Lg "veto b " & e.RaiseVeto
    XLog.Lg "mod " & e.RaiseMod("m")
    e.RaiseMany
    Ev2 = "ok"
    Exit Function
EH:
    Ev2 = Ex("ev2")
End Function

Public Function Ev3() As String
    On Error GoTo EH
    Dim e As VxSrc, a As VxSink, b As VxSink
    Set e = New VxSrc
    Set a = New VxSink: a.Tag = "A": Set a.Src = e
    Set b = New VxSink: b.Tag = "B": Set b.Src = e
    a.Boom = 11
    Dim r As String
    On Error Resume Next
    r = e.Raise1(1)
    XLog.Lg "after boom a: [" & r & "] " & Err.Number & " " & Err.Description
    Err.Clear
    a.Boom = 0
    b.Boom = 13
    r = e.Raise1(2)
    XLog.Lg "after boom b: [" & r & "] " & Err.Number & " " & Err.Description
    Err.Clear
    Ev3 = "ok"
    Exit Function
EH:
    Ev3 = Ex("ev3")
End Function

Public Function Ev4() As String
    On Error GoTo EH
    Dim e As VxSrc, a As VxSink, b As VxSink
    Set e = New VxSrc
    Set a = New VxSink: a.Tag = "A": Set a.Src = e
    Set b = New VxSink: b.Tag = "B": Set b.Src = e
    a.Clear = True
    XLog.Lg "clear in handler: " & e.Raise1(1)
    XLog.Lg "again: " & e.Raise1(2)
    Set e = Nothing
    XLog.Lg "e released"
    Set a = Nothing
    XLog.Lg "a released"
    Set b = Nothing
    XLog.Lg "b released"
    Ev4 = "ok"
    Exit Function
EH:
    Ev4 = Ex("ev4")
End Function

Public Function Ev5() As String
    On Error GoTo EH
    Dim e As VxSrc, a As VxSink
    Set e = New VxSrc
    Set a = New VxSink
    a.Tag = "A"
    Set a.Src = e
    Set e = Nothing
    XLog.Lg "e dropped, sink holds: " & (a.Src Is Nothing)
    XLog.Lg a.Src.Raise1(9)
    Set a = Nothing
    XLog.Lg "a dropped"
    Ev5 = "ok"
    Exit Function
EH:
    Ev5 = Ex("ev5")
End Function

Public Function Ev6() As String
    On Error GoTo EH
    Dim e As VxSrc
    Set e = New VxSrc
    Set GSink = New VxSink
    GSink.Tag = "G"
    Set GSink.Src = e
    XLog.Lg e.Raise1(1)
    Set GSink = Nothing
    XLog.Lg "global sink dropped"
    XLog.Lg e.Raise1(2)
    Ev6 = "ok"
    Exit Function
EH:
    Ev6 = Ex("ev6")
End Function

Public Function Xl0() As String
    Xl0 = Application.EnableEvents & "|" & Application.Interactive & "|" & Application.DisplayAlerts & "|" & Application.ScreenUpdating & "|" & Application.Calculation
End Function

Public Function Xl1() As String
    On Error GoTo EH
    Application.EnableEvents = True
    Dim x As VxXl, ws As Worksheet
    Set ws = ThisWorkbook.Worksheets("Sheet1")
    Set x = New VxXl
    x.Tag = "X"
    Set x.App = Application
    Set x.Wb = ThisWorkbook
    Set x.Ws = ws
    ws.Range("E5").Value = 1
    XLog.Lg "-- after E5"
    ws.Range("E5:E6").Value = 2
    XLog.Lg "-- after E5:E6"
    ThisWorkbook.Worksheets("S2").Range("C3").Value = 3
    XLog.Lg "-- after S2!C3"
    Application.EnableEvents = False
    ws.Range("E7").Value = 4
    Application.EnableEvents = True
    XLog.Lg "-- after disabled"
    ws.Range("F1").Formula = "=E5*2"
    XLog.Lg "-- after formula"
    ws.Range("E5").Value = 10
    XLog.Lg "-- after E5=10 value " & ws.Range("F1").Value
    Application.EnableEvents = False
    ws.Range("F1:F3").ClearContents
    ws.Range("E5:E7").ClearContents
    ThisWorkbook.Worksheets("S2").Range("C3").ClearContents
    Application.EnableEvents = True
    XLog.Lg "-- cleared"
    Set x.Ws = Nothing
    ws.Range("E5").Value = 1
    XLog.Lg "-- ws off"
    Set x.Wb = Nothing
    ws.Range("E5").Value = 2
    XLog.Lg "-- wb off"
    Application.EnableEvents = False
    ws.Range("E5").ClearContents
    Application.EnableEvents = True
    Xl1 = "ok"
    Exit Function
EH:
    Application.EnableEvents = True
    Xl1 = Ex("xl1")
End Function

Public Function Xl2() As String
    On Error GoTo EH
    Dim x As VxXl, y As VxXl, ws As Worksheet
    Set ws = ThisWorkbook.Worksheets("Sheet1")
    Set x = New VxXl: x.Tag = "X": Set x.Ws = ws: Set x.App = Application
    Set y = New VxXl: y.Tag = "Y": Set y.Ws = ws: Set y.App = Application
    ws.Range("G1").Value = 1
    XLog.Lg "-- two sinks"
    Set x = Nothing
    ws.Range("G1").Value = 2
    XLog.Lg "-- x dropped"
    Dim sh As Object
    Set y.Wb = ThisWorkbook
    Set sh = ThisWorkbook.Worksheets.Add(After:=ThisWorkbook.Worksheets(ThisWorkbook.Worksheets.Count))
    XLog.Lg "-- added " & sh.Name
    Application.DisplayAlerts = False
    sh.Delete
    Application.DisplayAlerts = True
    XLog.Lg "-- deleted"
    Application.EnableEvents = False
    ws.Range("G1").ClearContents
    Application.EnableEvents = True
    Xl2 = "ok"
    Exit Function
EH:
    Application.EnableEvents = True
    Application.DisplayAlerts = True
    Xl2 = Ex("xl2")
End Function

Public Function Xl3() As String
    On Error GoTo EH
    Set GXl = New VxXl
    GXl.Tag = "G"
    Set GXl.Ws = ThisWorkbook.Worksheets("Sheet1")
    ThisWorkbook.Worksheets("Sheet1").Range("H1").Value = 1
    Xl3 = "set"
    Exit Function
EH:
    Xl3 = Ex("xl3")
End Function

Public Function Xl4() As String
    On Error GoTo EH
    ThisWorkbook.Worksheets("Sheet1").Range("H1").Value = 2
    Application.EnableEvents = False
    ThisWorkbook.Worksheets("Sheet1").Range("H1").ClearContents
    Application.EnableEvents = True
    Set GXl = Nothing
    ThisWorkbook.Worksheets("Sheet1").Range("H1").Value = 3
    ThisWorkbook.Worksheets("Sheet1").Range("H1").ClearContents
    Xl4 = "done"
    Exit Function
EH:
    Application.EnableEvents = True
    Xl4 = Ex("xl4")
End Function

Private Function Mk(ByVal t As String) As VxLife
    Set Mk = New VxLife
    Mk.Tag = t
End Function

Public Function Lf1() As String
    Dim a As VxLife, b As VxLife, c As VxLife
    Set a = Mk("a")
    Set b = Mk("b")
    Set c = Mk("c")
    XLog.Lg "end of lf1"
    Lf1 = "ret"
End Function

Public Function Lf2() As String
    Dim a As VxLife, b As VxLife
    Set a = Mk("a")
    Set b = Mk("b")
    Set a.Child = b
    Set b = Nothing
    XLog.Lg "b local dropped"
    Set a = Nothing
    XLog.Lg "a dropped"
    Lf2 = "ok"
End Function

Public Function Lf3() As String
    Dim a As VxLife, b As VxLife
    Set a = Mk("a")
    Set b = Mk("b")
    Set a.Other = b
    Set b.Other = a
    Set a = Nothing
    Set b = Nothing
    XLog.Lg "cycle dropped"
    Lf3 = "ok"
End Function

Public Function Lf4() As String
    Dim a As VxLife
    Set a = Mk("first")
    Set a = Mk("second")
    XLog.Lg "reassigned"
    Set a = Nothing
    XLog.Lg "cleared"
    Lf4 = "ok"
End Function

Public Function Lf5() As String
    Dim a As VxLife
    Set a = New VxLife
    a.Tag = "x"
    Set a = New VxLife
    a.Tag = "y"
    XLog.Lg "two made"
    Dim tt As VxLife
    Set tt = New VxLife
    Lf5 = Mk("temp").Hello & "|" & tt.Hello
End Function

Public Function Lf6() As String
    Dim arr(1 To 3) As VxLife, i As Long
    For i = 1 To 3
        Set arr(i) = Mk("e" & i)
    Next
    XLog.Lg "filled"
    Erase arr
    XLog.Lg "erased"
    Dim d() As VxLife
    ReDim d(1 To 2)
    Set d(1) = Mk("d1")
    Set d(2) = Mk("d2")
    ReDim Preserve d(1 To 1)
    XLog.Lg "shrunk"
    ReDim d(1 To 2)
    XLog.Lg "redim"
    Lf6 = "ok"
End Function

Public Function Lf7() As String
    On Error GoTo EH
    Dim a As VxLife
    Set a = Mk("err")
    Dim x As Long
    x = 1 / 0
    Lf7 = "no"
    Exit Function
EH:
    XLog.Lg "handler " & Err.Number
    Lf7 = "handled"
End Function

Public Function Lf8() As String
    On Error GoTo EH
    Dim a As New VxLife
    XLog.Lg "declared, not used"
    Dim b As New VxLife
    b.Tag = "b"
    a.Tag = "a"
    XLog.Lg "used"
    Lf8 = "ok"
    Exit Function
EH:
    Lf8 = Ex("lf8")
End Function

Public Function Lf9() As String
    Set GL = Mk("global")
    Lf9 = "set"
End Function

Public Function Lf10() As String
    XLog.Lg "before"
    Set GL = Mk("global2")
    XLog.Lg "after replace"
    Set GL = Nothing
    XLog.Lg "after nothing"
    Lf10 = "ok"
End Function

Public Function Lf11() As String
    Dim c As New Collection, i As Long
    For i = 1 To 3
        c.Add Mk("c" & i), "k" & i
    Next
    XLog.Lg "added"
    c.Remove 2
    XLog.Lg "removed 2"
    c.Remove "k1"
    XLog.Lg "removed k1"
    Set c = Nothing
    XLog.Lg "collection gone"
    Lf11 = "ok"
End Function

Private Sub TakeByVal(ByVal o As VxLife)
    XLog.Lg "byval " & o.Tag
End Sub

Private Sub TakeByRef(ByRef o As VxLife)
    Set o = Mk("replaced")
    XLog.Lg "byref replaced"
End Sub

Public Function Lf12() As String
    Dim a As VxLife
    Set a = Mk("orig")
    TakeByVal a
    XLog.Lg "after byval"
    TakeByRef a
    XLog.Lg "after byref " & a.Tag
    Lf12 = "ok"
End Function

Public Function Lf13() As String
    With Mk("w")
        XLog.Lg "in with " & .Hello
    End With
    XLog.Lg "after with"
    Dim v As Variant
    Set v = Mk("v")
    v = Empty
    XLog.Lg "variant emptied"
    Lf13 = "ok"
End Function

Public Function Pr1() As String
    On Error GoTo EH
    Dim p As VxProp, x As Variant
    Set p = New VxProp
    p = 5
    x = p
    XLog.Lg "default " & x & " " & TypeName(x) & " " & p.Value & " " & p
    p.Value = "str"
    XLog.Lg "now " & p
    p = Null
    XLog.Lg "null " & IsNull(p.Value) & TypeName(p.Value)
    p = Empty
    XLog.Lg "empty " & TypeName(p.Value)
    p.Value = Array(1, 2)
    XLog.Lg "array " & TypeName(p.Value) & UBound(p.Value)
    Dim q As VxProp
    Set q = New VxProp
    q = 7
    p = q
    XLog.Lg "p=q " & p & " " & TypeName(p.Value)
    Set p.Value = q
    XLog.Lg "set value " & TypeName(p.Value)
    Set p.Obj = q
    XLog.Lg "obj " & TypeName(p.Obj) & " " & (p.Obj Is q) & " " & p.Obj
    Pr1 = "ok"
    Exit Function
EH:
    Pr1 = Ex("pr1")
End Function

Public Function Pr2() As String
    On Error Resume Next
    Dim p As VxProp, s As String, v As Variant
    Set p = New VxProp
    p.Arr(1) = "a"
    p.Arr(3) = "c"
    s = p.Arr(1) & p.Arr(2) & p.Arr(3) & "|"
    p.Arr(4) = "x"
    s = s & "oob " & Err.Number & "|"
    Err.Clear
    v = p.WriteOnly
    s = s & "wo get " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    p.WriteOnly = 3
    s = s & "wo let " & Err.Number & "|"
    Err.Clear
    v = p.ReadOnly1
    s = s & "ro " & v & "|"
    Dim o As Object
    Set o = p
    o.ReadOnly1 = 3
    s = s & "ro let " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    s = s & p.Fr(4) & p.FrP & p.CallPriv & "|"
    v = o.Fr(1)
    s = s & "friend late " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    v = o.Priv
    s = s & "priv late " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    v = o.CallPriv
    s = s & "callpriv late " & v & Err.Number & "|"
    Pr2 = s
End Function

Public Function Pr3() As String
    On Error Resume Next
    Dim p As VxProp, s As String, v As Variant
    Set p = New VxProp
    s = p.Opt & ";" & p.Opt(1) & ";" & p.Opt(2, 3) & ";" & p.Opt(, 1) & ";" & p.Opt(b:=1) & "|"
    s = s & p.PArr & ";" & p.PArr(1) & ";" & p.PArr(1, 2, 3) & ";" & p.PArr(Empty, Null) & "|"
    s = s & TypeName(p.MeSelf) & (p.MeSelf Is p) & "|"
    Dim o As Object
    Set o = p
    v = CallByName(o, "Value", VbLet, 12)
    s = s & "cbn let " & Err.Number & p & "|"
    Err.Clear
    v = CallByName(o, "Value", VbGet)
    s = s & "cbn get " & v & Err.Number & "|"
    Err.Clear
    CallByName o, "Obj", VbSet, p
    s = s & "cbn set " & Err.Number & (p.Obj Is p) & "|"
    Err.Clear
    v = CallByName(o, "Opt", VbMethod, 8)
    s = s & "cbn method " & v & Err.Number & "|"
    Err.Clear
    v = CallByName(o, "Nope", VbMethod)
    s = s & "cbn nope " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    v = CallByName(o, "Fr", VbMethod, 1)
    s = s & "cbn friend " & Err.Number & "|"
    Err.Clear
    v = CallByName(o, "Value", VbMethod)
    s = s & "cbn value method " & Err.Number & "|"
    Err.Clear
    v = CallByName(o, "ReadOnly1", VbLet, 1)
    s = s & "cbn ro let " & Err.Number & ":" & Err.Description & "|"
    Pr3 = s
End Function

Public Function Sc1() As String
    On Error Resume Next
    Dim s As String, v As Variant
    s = VxAux.PubF & VxAux.ViaPriv & VxAux.PubC & "|"
    s = s & VxAux.Counter & "," & VxAux.Counter & "," & VxAux.PubV & "|"
    v = Application.Run("VxAux.PrivF")
    s = s & "run priv " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    v = Application.Run("PrivF")
    s = s & "run priv bare " & Err.Number & "|"
    Err.Clear
    v = Application.Run("VxAux.PubF")
    s = s & "run pub " & v & Err.Number & "|"
    Err.Clear
    v = Application.Run("VxOpm.OpmF", 2)
    s = s & "run opm " & v & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    v = Application.Run("OpmF", 2)
    s = s & "run opm bare " & v & Err.Number & "|"
    Err.Clear
    v = Application.Run("VxAux.PubC")
    s = s & "run const " & Err.Number & "|"
    Err.Clear
    v = Application.Run("VxAux.PubV")
    s = s & "run var " & Err.Number & "|"
    Err.Clear
    v = Application.Run("R7Nomod.F")
    s = s & "run nomod " & Err.Number & "|"
    Err.Clear

    Sc1 = s
End Function

Public Function Rn1() As String
    Dim s As String, i As Long
    Rnd -1
    Randomize 7
    For i = 1 To 4
        s = s & Rnd & ","
    Next
    Rnd -1
    Randomize 7
    s = s & "|" & Rnd & "|" & Rnd(0) & "|" & Rnd(0) & "|" & Rnd(1) & "|" & Rnd(-7) & "|" & Rnd(-7) & "|" & Rnd
    Rn1 = s
End Function

Public Function Rn2() As String
    Dim s As String, i As Long
    Rnd -1
    Randomize 123456
    For i = 1 To 3
        s = s & Int(Rnd * 100) & ","
    Next
    Randomize 123456
    s = s & "|" & Int(Rnd * 100) & "|"
    Rnd -2
    s = s & Rnd & "|"
    Randomize -5
    s = s & Rnd & "|"
    Randomize 0
    s = s & Rnd & "|"
    Randomize 3.7
    s = s & Rnd
    Rn2 = s
End Function

Public Function Rn3() As String
    Dim t1 As Single, t2 As Single, s As String
    t1 = Timer
    t2 = Timer
    s = TypeName(t1) & (t1 >= 0) & (t1 < 86400) & (t2 >= t1)
    Dim r As Single
    Randomize
    r = Rnd
    s = s & (r >= 0) & (r < 1) & TypeName(r)
    Rn3 = s
End Function

Public Function Tm1() As String
    On Error Resume Next
    Dim t As Date, s As String
    t = Now + 1
    Application.OnTime t, "VxMain.Nop"
    s = "ontime " & Err.Number & "|"
    Err.Clear
    Application.OnTime t, "VxMain.Nop", , False
    s = s & "cancel " & Err.Number & "|"
    Err.Clear
    Application.OnTime t, "VxMain.Nop", , False
    s = s & "cancel again " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Application.OnTime t + 1, "NoSuchProc", , False
    s = s & "cancel none " & Err.Number & "|"
    Err.Clear
    Application.OnTime t, "NoSuchProc"
    s = s & "nosuch " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Application.OnTime t, "NoSuchProc", , False
    Err.Clear
    Application.OnTime "25:00", "VxMain.Nop"
    s = s & "badtime " & Err.Number & "|"
    Err.Clear
    Application.OnTime t, "VxMain.Nop", t + 2
    s = s & "latest " & Err.Number & "|"
    Err.Clear
    Application.OnTime t, "VxMain.Nop", t + 2, False
    Err.Clear
    Tm1 = s
End Function

Public Function Ky1() As String
    On Error Resume Next
    Dim s As String
    Application.OnKey "^q", "VxMain.Nop"
    s = "set " & Err.Number & "|"
    Err.Clear
    Application.OnKey "^q"
    s = s & "reset " & Err.Number & "|"
    Err.Clear
    Application.OnKey "^q", ""
    s = s & "empty " & Err.Number & "|"
    Err.Clear
    Application.OnKey "{BADKEY}", "VxMain.Nop"
    s = s & "bad " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Application.OnKey "", "VxMain.Nop"
    s = s & "blank " & Err.Number & "|"
    Err.Clear
    Application.OnKey "a", "NoSuchProc"
    s = s & "noproc " & Err.Number & "|"
    Err.Clear
    Application.OnKey "a"
    Application.OnKey "{F9}", "VxMain.Nop"
    Application.OnKey "{F9}"
    s = s & "f9 " & Err.Number
    Ky1 = s
End Function

Private Sub Boom(ByVal n As Long)
    Err.Raise n, "BoomSrc", "boom " & n
End Sub

Private Sub Quiet()
    On Error Resume Next
    Dim x As Long
    x = 1 / 0
    XLog.Lg "quiet saw " & Err.Number
End Sub

Private Sub Quiet2()
    On Error GoTo EH
    Dim x As Long
    x = 1 / 0
    Exit Sub
EH:
    XLog.Lg "quiet2 handled " & Err.Number
End Sub

Private Function Deep(ByVal d As Long) As Long
    If d = 0 Then
        Err.Raise 1234, "deep", "bottom"
    Else
        Deep = Deep(d - 1)
    End If
End Function

Public Function Er1() As String
    On Error Resume Next
    Dim s As String
    Boom 11
    s = "a " & Err.Number & ":" & Err.Description & ":" & Err.Source & "|"
    Err.Clear
    Quiet
    s = s & "b " & Err.Number & "|"
    Err.Clear
    Quiet2
    s = s & "c " & Err.Number & "|"
    Dim x As Long
    x = Deep(3)
    s = s & "d " & Err.Number & ":" & Err.Description & ":" & Err.Source & " x=" & x & "|"
    Err.Clear
    Boom 5
    On Error GoTo 0
    s = s & "e " & Err.Number & "|"
    Er1 = s
End Function

Public Function Er2() As String
    On Error GoTo H1
    Boom 7
    Er2 = "no"
    Exit Function
H1:
    Dim s As String
    s = "h1 " & Err.Number & Err.Description & "|"
    On Error GoTo -1
    On Error Resume Next
    Boom 10
    s = s & "rn " & Err.Number & "|"
    Err.Clear
    s = s & "cleared " & Err.Number & "|"
    Er2 = s
End Function

Public Function Er3() As String
    Dim s As String
    On Error Resume Next
    Err.Raise 0
    s = "r0 " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Err.Raise 65536
    s = s & "r65536 " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Err.Raise -1
    s = s & "rneg " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Err.Raise 1000
    s = s & "r1000 " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Err.Raise 5, "mysrc"
    s = s & "r5 " & Err.Number & ":" & Err.Description & ":" & Err.Source & "|"
    Err.Clear
    Err.Raise vbObjectError + 5
    s = s & "robj " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Err.Raise 11, , "custom"
    s = s & "r11 " & Err.Number & ":" & Err.Description & "|"
    Err.Clear
    Error 13
    s = s & "error13 " & Err.Number & ":" & Err.Description & ":" & Err.Source & "|"
    Err.Clear
    Err.Raise 5, , , "h.hlp", 77
    s = s & "help " & Err.HelpFile & Err.HelpContext & "|"
    Err.Clear
    s = s & "clear " & Err.Number & "[" & Err.Description & "][" & Err.Source & "][" & Err.HelpFile & "]" & Err.HelpContext & Err.LastDllError & "|"
    Err.Raise 6
    s = s & "r6 " & Err.Number & "|"
    Er3 = s
End Function

Public Function Er4() As String
    Dim s As String, x As Long
    On Error Resume Next
    x = 5
    x = x / 0 + 1
    s = "a " & x & ":" & Err.Number & "|"
    Err.Clear
    x = 7
    x = Deep(1) + 10
    s = s & "b " & x & ":" & Err.Number & "|"
    Err.Clear
    s = s & "c " & Deep(0) & ":" & Err.Number & "|"
    Err.Clear
    If 1 / 0 = 1 Then s = s & "then|" Else s = s & "else|"
    s = s & "d " & Err.Number & "|"
    Err.Clear
    Dim i As Long
    For i = 1 To 3
        x = i \ (i - 2)
        s = s & i & ":" & Err.Number & " "
        Err.Clear
    Next
    Select Case 1 / 0
    Case 1: s = s & "|c1"
    Case Else: s = s & "|ce"
    End Select
    s = s & "|e " & Err.Number
    Er4 = s
End Function

Public Function Er5() As String
    Dim s As String
    On Error Resume Next
10  Boom 3
20  s = s & "erl " & Erl & ","
30  Err.Clear
40  s = s & Erl & ","
50  Boom 4
    s = s & Erl & "|"
    Er5 = s
End Function

Public Function Er6() As String
    Dim s As String, n As Long
    On Error GoTo EH
    n = n + 1
    Boom 20 + n
    s = s & "after boom|"
    Er6 = s
    Exit Function
EH:
    s = s & "eh" & n & ":" & Err.Number & " "
    If n < 3 Then
        n = n + 1
        Resume Next
    End If
    s = s & "|"
    Resume Done
Done:
    Er6 = s & "done"
End Function

Public Function Dbg1() As String
    Dim s As String, r As Long
    Debug.Print "hello"
    Debug.Print 1, 2; 3
    Debug.Print
    Debug.Print Tab(5); "x"; Spc(2); "y"
    Debug.Print Null, Empty, True
    Debug.Assert True
    r = DoEvents
    s = TypeName(r) & r
    DoEvents
    s = s & "|" & TypeName(DoEvents)
    Dim o As Object
    On Error Resume Next
    Debug.Print o.x
    s = s & "|" & Err.Number
    Dbg1 = s
End Function
