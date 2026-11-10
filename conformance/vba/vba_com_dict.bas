Attribute VB_Name = "VbaComDict"
Option Explicit

Private Function Fmt(v As Variant) As String
    If IsError(v) Then
        Fmt = "err"
    ElseIf IsEmpty(v) Then
        Fmt = "<empty>"
    Else
        Fmt = TypeName(v) & ":" & CStr(v)
    End If
End Function

Public Function DictT1() As String
    Dim d As Object, s As String, k As Variant
    Set d = CreateObject("Scripting.Dictionary")
    d.Add "a", 1
    d.Add "b", "two"
    d("c") = 3
    s = d.Count & "|" & d.Exists("a") & "|" & d.Exists("A") & "|" & Fmt(d("b")) & "|" & Fmt(d.Item("c"))
    For Each k In d.Keys: s = s & "|k" & Fmt(k): Next
    For Each k In d.Items: s = s & "|i" & Fmt(k): Next
    For Each k In d: s = s & "|e" & Fmt(k): Next
    s = s & "|" & TypeName(d) & "|" & d.CompareMode
    DictT1 = s
End Function

Public Function DictT2() As String
    Dim d As Object, s As String
    Set d = CreateObject("Scripting.Dictionary")
    s = d.Count & "|" & Fmt(d("missing")) & "|" & d.Count & "|" & d.Exists("missing")
    d("missing") = 5
    s = s & "|" & Fmt(d("missing")) & "|" & d.Count
    d.Key("missing") = "other"
    s = s & "|" & d.Exists("missing") & "|" & Fmt(d("other"))
    d.Remove "other"
    s = s & "|" & d.Count
    d.Add 1, "n": d.Add "1", "s": d.Add 1.5, "f": d.Add True, "b"
    s = s & "|" & d.Count & "|" & Fmt(d(1)) & "|" & Fmt(d("1")) & "|" & Fmt(d(1.5)) & "|" & Fmt(d(True))
    d.RemoveAll
    DictT2 = s & "|" & d.Count
End Function

Public Function DictT3() As String
    Dim d As Object, s As String
    Set d = CreateObject("Scripting.Dictionary")
    d.CompareMode = 1
    d.Add "Abc", 1
    s = d.Exists("ABC") & "|" & Fmt(d("aBc")) & "|" & d.Count
    d("ABC") = 2
    s = s & "|" & d.Count & "|" & Fmt(d.Keys()(0)) & "|" & Fmt(d("abc"))
    DictT3 = s
End Function

Private Function Trap(code As Long, what As String) As String
    Trap = what & "=" & code
End Function

Public Function DictT4() As String
    Dim d As Object, s As String
    Set d = CreateObject("Scripting.Dictionary")
    d.Add "a", 1
    On Error Resume Next
    d.Add "a", 2
    s = Trap(Err.Number, "dup"): Err.Clear
    d.Remove "zz"
    s = s & "|" & Trap(Err.Number, "rm"): Err.Clear
    d.CompareMode = 1
    s = s & "|" & Trap(Err.Number, "cm"): Err.Clear
    d.Key("zz") = "q"
    s = s & "|" & Trap(Err.Number, "key"): Err.Clear
    d.Add "b"
    s = s & "|" & Trap(Err.Number, "arity"): Err.Clear
    d.Nope
    s = s & "|" & Trap(Err.Number, "nope"): Err.Clear
    d.Count = 3
    s = s & "|" & Trap(Err.Number, "ro"): Err.Clear
    d.Add Array(1), 1
    s = s & "|" & Trap(Err.Number, "arrkey"): Err.Clear
    Dim o As Object
    Set o = CreateObject("No.Such.Class")
    s = s & "|" & Trap(Err.Number, "noclass") & "|" & Err.Description: Err.Clear
    Set o = GetObject("C:\no\such\file.xlsx")
    s = s & "|" & Trap(Err.Number, "nofile"): Err.Clear
    DictT4 = s
End Function

Public Function DictT5() As String
    Dim d As Object, e As Object, s As String
    Set d = CreateObject("Scripting.Dictionary")
    Set e = d
    e.Add "x", 1
    d.Add "obj", d
    s = d.Count & "|" & (d Is e) & "|" & TypeName(d("obj")) & "|" & d("obj").Count
    Dim w As Object
    Set w = CreateObject("Scripting.Dictionary")
    w.Add "r", Application.Caller
    DictT5 = s
End Function
