Attribute VB_Name = "VbaClass"
Option Explicit

Private Type Pt
    X As Long
    Y As Long
    Name As String
End Type

Private Type Box
    TopLeft As Pt
    Size As Double
End Type

Public Gc As ClsCounter

Public Function NextId() As Long
    On Error GoTo EH
    Static c As Long
    c = c + 1
    NextId = c
    Exit Function
EH:
    NextId = -1
End Function

Public Function T1Lifetime() As String
    On Error GoTo EH
    Dim a As ClsCounter
    Set a = New ClsCounter
    a.Name = "a"
    VcLog.Lg "made a"
    Set a = Nothing
    VcLog.Lg "after a"
    Dim b As ClsCounter, c As ClsCounter
    Set b = New ClsCounter
    b.Name = "b"
    Set c = b
    Set b = Nothing
    VcLog.Lg "b dropped, c holds"
    Set c = Nothing
    VcLog.Lg "c dropped"
    T1Lifetime = VcLog.GetLog()
    Exit Function
EH:
    T1Lifetime = "err " & Err.Number & ":" & VcLog.GetLog()
End Function

Public Function T2Scope() As String
    On Error GoTo EH
    Inner
    VcLog.Lg "after inner"
    T2Scope = VcLog.GetLog()
    Exit Function
EH:
    T2Scope = "err " & Err.Number
End Function

Private Sub Inner()
    On Error GoTo EH
    Dim a As New ClsCounter
    a.Name = "inner"
    VcLog.Lg "in inner " & a.Count
    Exit Sub
EH:
    VcLog.Lg "inner error " & Err.Number
End Sub

Public Function T3AsNew() As String
    On Error GoTo EH
    Dim a As New ClsCounter
    VcLog.Lg "declared"
    a.Name = "asnew"
    VcLog.Lg "used " & a.Count
    Set a = Nothing
    VcLog.Lg "set nothing"
    VcLog.Lg "reuse " & a.Count
    a.Name = "again"
    T3AsNew = VcLog.GetLog()
    Exit Function
EH:
    T3AsNew = "err " & Err.Number & ":" & VcLog.GetLog()
End Function

Public Function T4Props() As String
    On Error GoTo EH
    Dim a As ClsCounter
    Set a = New ClsCounter
    a.Name = "p"
    a.Count = 5
    a.Inc
    a.Inc
    VcLog.Lg "count " & a.Count
    VcLog.Lg "default " & a
    VcLog.Lg "default2 " & a.Item
    VcLog.Lg "self " & a.Self.Count
    Dim h As ClsHolder
    Set h = New ClsHolder
    Set h.Target = a
    VcLog.Lg "via holder " & h.Target.Count
    Set a = Nothing
    VcLog.Lg "a dropped"
    Set h = Nothing
    VcLog.Lg "h dropped"
    T4Props = VcLog.GetLog()
    Exit Function
EH:
    T4Props = "err " & Err.Number & ":" & VcLog.GetLog()
End Function

Public Function T5Is() As String
    On Error GoTo EH
    Dim a As ClsCounter, b As ClsCounter, c As ClsCounter
    Set a = New ClsCounter
    Set b = a
    Set c = New ClsCounter
    VcLog.Lg "a is b " & (a Is b)
    VcLog.Lg "a is c " & (a Is c)
    VcLog.Lg "nothing " & (a Is Nothing)
    Set c = Nothing
    VcLog.Lg "c nothing " & (c Is Nothing)
    VcLog.Lg "typename " & TypeName(a) & " " & TypeName(c)
    VcLog.Lg "typeof " & (TypeOf a Is ClsCounter)
    Dim o As Object
    Set o = a
    VcLog.Lg "obj typeof " & (TypeOf o Is ClsCounter) & (TypeOf o Is ClsHolder)
    T5Is = VcLog.GetLog()
    Exit Function
EH:
    T5Is = "err " & Err.Number & ":" & VcLog.GetLog()
End Function

Public Function T6Implements() As String
    On Error GoTo EH
    Dim s As ClsSquare, sh As ClsShape
    Set s = New ClsSquare
    s.Side = 3
    Set sh = s
    VcLog.Lg "area " & sh.Area & " " & sh.Label & " " & s.Own
    VcLog.Lg "typeof shape " & (TypeOf s Is ClsShape) & " square " & (TypeOf sh Is ClsSquare)
    VcLog.Lg TypeName(sh)
    T6Implements = VcLog.GetLog()
    Exit Function
EH:
    T6Implements = "err " & Err.Number & ":" & VcLog.GetLog()
End Function

Public Function T7Events() As String
    On Error GoTo EH
    Dim e As ClsEvt, l1 As ClsListener, l2 As ClsListener
    Set e = New ClsEvt
    Set l1 = New ClsListener
    l1.Tag = "L1"
    Set l1.Src = e
    VcLog.Lg "fire1 " & e.Fire(1)
    Set l2 = New ClsListener
    l2.Tag = "L2"
    Set l2.Src = e
    VcLog.Lg "fire3 " & e.Fire(3)
    Set l1.Src = Nothing
    VcLog.Lg "fire4 " & e.Fire(4)
    T7Events = VcLog.GetLog()
    Exit Function
EH:
    T7Events = "err " & Err.Number & ":" & VcLog.GetLog()
End Function

Public Function T8Udt() As String
    On Error GoTo EH
    Dim p As Pt, b As Box, q As Pt
    p.X = 3: p.Y = 4: p.Name = "p"
    q = p
    q.X = 9
    b.TopLeft = p
    b.Size = 2.5
    b.TopLeft.Y = 7
    T8Udt = p.X & "," & p.Y & "," & q.X & "," & b.TopLeft.Y & "," & b.Size & "," & b.TopLeft.Name
    Exit Function
EH:
    T8Udt = "err " & Err.Number
End Function

Public Function T9Static() As String
    On Error GoTo EH
    Dim i As Long, s As String
    For i = 1 To 3
        s = s & NextId() & ","
    Next i
    T9Static = s
    Exit Function
EH:
    T9Static = "err " & Err.Number
End Function

Public Function T10Global() As String
    On Error GoTo EH
    If Gc Is Nothing Then Set Gc = New ClsCounter
    Gc.Name = "global"
    Gc.Inc
    T10Global = "count " & Gc.Count & ":" & VcLog.GetLog()
    Exit Function
EH:
    T10Global = "err " & Err.Number
End Function

Public Function T11DropGlobal() As String
    On Error GoTo EH
    Set Gc = Nothing
    T11DropGlobal = VcLog.GetLog()
    Exit Function
EH:
    T11DropGlobal = "err " & Err.Number
End Function

Public Function T12TempAndWith() As String
    On Error GoTo EH
    VcLog.Lg "count " & New_Counter().Count
    VcLog.Lg "after temp"
    With New ClsCounter
        .Name = "with"
        VcLog.Lg "in with " & .Count
    End With
    VcLog.Lg "after with"
    T12TempAndWith = VcLog.GetLog()
    Exit Function
EH:
    T12TempAndWith = "err " & Err.Number & ":" & VcLog.GetLog()
End Function

Private Function New_Counter() As ClsCounter
    On Error GoTo EH
    Set New_Counter = New ClsCounter
    New_Counter.Name = "temp"
    Exit Function
EH:
    VcLog.Lg "newcounter error " & Err.Number
End Function

Public Function T13Errors() As String
    On Error GoTo EH
    Dim a As ClsCounter
    On Error Resume Next
    VcLog.Lg "unset " & a.Count
    VcLog.Lg "err91 " & Err.Number
    Err.Clear
    Dim o As Object
    Set o = New ClsCounter
    VcLog.Lg "nomember " & o.Missing
    VcLog.Lg "err438 " & Err.Number
    Err.Clear
    On Error GoTo EH
    T13Errors = VcLog.GetLog()
    Exit Function
EH:
    T13Errors = "err " & Err.Number & ":" & VcLog.GetLog()
End Function

Public Function T14Collection() As String
    On Error GoTo EH
    Dim col As New Collection, a As ClsCounter, i As Long
    For i = 1 To 2
        Set a = New ClsCounter
        a.Name = "c" & i
        col.Add a
    Next i
    Set a = Nothing
    VcLog.Lg "in collection " & col.Count
    Set col = Nothing
    VcLog.Lg "collection dropped"
    T14Collection = VcLog.GetLog()
    Exit Function
EH:
    T14Collection = "err " & Err.Number & ":" & VcLog.GetLog()
End Function
