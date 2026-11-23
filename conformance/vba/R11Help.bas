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
Private Function Dmp(v As Variant) As String
    Dim s As String, i As Long
    If IsArray(v) Then
        On Error Resume Next
        s = "A" & LBound(v) & ":" & UBound(v) & "["
        If Err.Number <> 0 Then Dmp = "A<" & Err.Number & ">": Exit Function
        On Error GoTo 0
        For i = LBound(v) To UBound(v)
            s = s & Dmp(v(i)) & ","
        Next i
        Dmp = s & "]"
    ElseIf IsNull(v) Then
        Dmp = "Null"
    ElseIf IsEmpty(v) Then
        Dmp = "Empty"
    ElseIf IsObject(v) Then
        Dmp = "<" & TypeName(v) & ">"
    ElseIf VarType(v) = vbDouble Or VarType(v) = vbSingle Then
        Dmp = TypeName(v) & ":" & CStr(v) & "~" & CStr(v - CDbl(CStr(v)))
    ElseIf VarType(v) = vbError Then
        Dmp = "Err:" & CLng(v)
    Else
        Dmp = TypeName(v) & ":" & Hx(CStr(v))
    End If
End Function
Private Function ErrS() As String
    ErrS = Err.Number & "/" & Err.Source & "/" & Err.Description
    Err.Clear
End Function
Private Function PA(ParamArray a() As Variant) As String
    Dim i As Long, s As String
    s = LBound(a) & ":" & UBound(a) & "["
    For i = LBound(a) To UBound(a)
        s = s & Dmp(a(i)) & ","
    Next i
    PA = s & "]"
End Function
Private Function Opt(Optional ByVal a As Variant, Optional ByVal b As Long = 7, Optional ByVal c As String = "d") As String
    Opt = IsMissing(a) & "|" & b & "|" & c
End Function
Private Sub Bump(ByRef x As Variant)
    x = x + 1
End Sub
Private Sub BumpL(ByRef x As Long)
    x = x + 1
End Sub
Private Function InnerErr1() As String
    On Error Resume Next
    Err.Raise 5
    InnerErr1 = "in" & Err.Number
End Function
