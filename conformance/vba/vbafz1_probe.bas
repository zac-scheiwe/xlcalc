Attribute VB_Name = "ModVbFz1Probe"
Option Explicit
Option Compare Text
Private Function Hx(ByVal s As String) As String
    Dim i As Long, t As String, bad As Boolean
    For i = 1 To Len(s)
        If AscW(Mid$(s, i, 1)) < 32 Or AscW(Mid$(s, i, 1)) > 126 Then bad = True
    Next i
    If Not bad Then
        Hx = "'" & s & "'"
    Else
        For i = 1 To Len(s)
            t = t & Hex$(AscW(Mid$(s, i, 1)) And &HFFFF&) & " "
        Next i
        Hx = "<" & t & ">"
    End If
End Function
Private Function D8(ByVal d As Double) As String
    Dim r As Double
    On Error Resume Next
    r = d - CDbl(CStr(d))
    If Err.Number <> 0 Then D8 = CStr(d) & "~?" Else D8 = CStr(d) & "~" & CStr(r)
End Function
Private Function Dmp(v As Variant) As String
    Dim s As String, i As Long, j As Long, l2 As Long, u2 As Long, nd As Long
    If IsObject(v) Then
        If v Is Nothing Then Dmp = "<Nothing>" Else Dmp = "<" & TypeName(v) & ">"
    ElseIf IsArray(v) Then
        On Error Resume Next
        s = TypeName(v) & LBound(v) & ":" & UBound(v)
        If Err.Number <> 0 Then Dmp = "A<" & Err.Number & ">": Exit Function
        nd = 1
        l2 = LBound(v, 2): u2 = UBound(v, 2)
        If Err.Number = 0 Then nd = 2: s = s & "," & l2 & ":" & u2
        Err.Clear
        s = s & "["
        If nd = 1 Then
            For i = LBound(v) To UBound(v)
                s = s & Dmp(v(i)) & ","
            Next i
        Else
            For i = LBound(v) To UBound(v)
                For j = l2 To u2
                    s = s & Dmp(v(i, j)) & ","
                Next j
                s = s & "/"
            Next i
        End If
        Dmp = s & "]"
    ElseIf IsNull(v) Then
        Dmp = "Null"
    ElseIf IsEmpty(v) Then
        Dmp = "Empty"
    ElseIf VarType(v) = vbDouble Or VarType(v) = vbSingle Then
        Dmp = TypeName(v) & ":" & D8(v)
    ElseIf VarType(v) = vbDate Then
        Dmp = "Date:" & D8(CDbl(v))
    ElseIf VarType(v) = vbError Then
        Dmp = "Err:" & CLng(v)
    Else
        Dmp = TypeName(v) & ":" & Hx(CStr(v))
    End If
End Function
Private Function Ap(v As Variant) As String
    If Err.Number <> 0 Then
        Ap = "#E" & Err.Number & ";"
        Err.Clear
    Else
        Ap = Dmp(v) & ";"
    End If
End Function
Private Function ErrS() As String
    ErrS = Err.Number & "/" & Err.Source & "/" & Hx(Err.Description) & ";"
    Err.Clear
End Function
Public Function VbFz1Probe(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1Probe = C0()
    Case 1: VbFz1Probe = C1()
    Case 2: VbFz1Probe = C2()
    Case 3: VbFz1Probe = C3()
    Case 4: VbFz1Probe = C4()
    Case 5: VbFz1Probe = C5()
    Case 6: VbFz1Probe = C6()
    Case 7: VbFz1Probe = C7()
    End Select
    Exit Function
EH:
    VbFz1Probe = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function Sx(ByVal t As Long) As Long
    If t = 2 Then Err.Raise 5
    Sx = 2
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Concat(#1/2/2000#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Concat(#1/2/2000 3:04:05 PM#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Concat(CDate(0.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Concat(CCur(-1234.5)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Concat(CCur(0)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Len(CCur(2.25)): s = s & Ap(v)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(2,""A"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(2,""A1"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(2,""A1:A3"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(2,""x1"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(2,""1:2"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Text(2, "A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Text(2, "B"): s = s & Ap(v)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Index(2.5, Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Index(CInt(3), 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Index(CLng(3), 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Index("a", Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Index(True, Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Index(-1, 0, 1): s = s & Ap(v)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.VLookup(5, Range("Sheet1!A1:B6"), Range("Sheet1!A2:A3")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.VLookup(5, Range("Sheet1!A1:B6"), Range("Sheet1!A5:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.VLookup(5, Range("Sheet1!A1:B6"), Range("Sheet1!C2:C3")): s = s & Ap(v)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, i As Variant, j As Long, t As Long, n As Long, z As Double
    On Error Resume Next
    n = 0: i = Empty
    For i = 1 To 1 / z
        n = n + 1: If n > 4 Then Exit For
    Next i
    s = s & Ap(i) & n & ";"
    Err.Clear: n = 0
    For j = 1 / z To 3
        n = n + 1: If n > 4 Then Exit For
    Next j
    s = s & j & "," & n & ";"
    Err.Clear
    For t = 1 To 2
        n = 0
        For j = 1 To 7 Step Sx(t)
            n = n + 1: If n > 4 Then Exit For
        Next j
        s = s & j & "," & n & ";"
        Err.Clear
    Next t
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant, a() As Long
    On Error Resume Next
    v = Array(1, 2)
    ReDim v(3 To 1)
    s = s & Err.Number & ";": Err.Clear
    s = s & IsArray(v) & ";" & UBound(v) & ";" & Err.Number & ";": Err.Clear
    ReDim a(1 To 2)
    ReDim a(5 To 2)
    s = s & Err.Number & ";": Err.Clear
    s = s & UBound(a) & ";" & Err.Number & ";": Err.Clear
    C5 = s
End Function
Private Function C6() As String
    Dim s As String
    On Error Resume Next
    s = s & (ChrW(&H152) Like "oe") & ";"
    s = s & (ChrW(&HC6) Like "?") & ";"
    s = s & (ChrW(&HC6) Like "??") & ";"
    s = s & ("ae" Like ChrW(&HC6)) & ";"
    s = s & (ChrW(&HDF) Like "?") & ";"
    s = s & (ChrW(&HE6) Like "a*") & ";"
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(0.6, "0,;;""z""0.0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(79, "#,##0,,;;""z""0.00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(-51.563, "+0.###;#,##0,,.000"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.007, "0.0;;""z""0.0000"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(-0.6, "0,;0,;""z""0.0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.3, """x""?);;0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(13, "0;E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(-13, "0;E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("32768", """T"".hh/d"): s = s & Ap(v)
    C7 = s
End Function
