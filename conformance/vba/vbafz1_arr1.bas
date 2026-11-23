Attribute VB_Name = "ModVbFz1Arr1"
Option Base 1
Option Explicit
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
Private Sub ModL(ByRef x() As Long, ByVal n As Long)
    x(LBound(x)) = x(LBound(x)) + 100
    ReDim Preserve x(LBound(x) To UBound(x) + n)
End Sub
Private Sub ModV(ByVal x As Variant)
    x(LBound(x)) = "changed"
End Sub
Private Sub ModVR(ByRef x As Variant)
    x(LBound(x)) = "changed"
End Sub
Private Function MkL(ByVal lo As Long, ByVal hi As Long) As Long()
    Dim r() As Long, i As Long
    ReDim r(lo To hi)
    For i = lo To hi: r(i) = i * 3: Next i
    MkL = r
End Function
Private Function SumV(x As Variant) As Variant
    Dim e As Variant
    For Each e In x: SumV = SumV + e: Next e
End Function
Public Function VbFz1Arr1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1Arr1 = C0()
    Case 1: VbFz1Arr1 = C1()
    Case 2: VbFz1Arr1 = C2()
    Case 3: VbFz1Arr1 = C3()
    Case 4: VbFz1Arr1 = C4()
    Case 5: VbFz1Arr1 = C5()
    Case 6: VbFz1Arr1 = C6()
    Case 7: VbFz1Arr1 = C7()
    Case 8: VbFz1Arr1 = C8()
    Case 9: VbFz1Arr1 = C9()
    Case 10: VbFz1Arr1 = C10()
    Case 11: VbFz1Arr1 = C11()
    Case 12: VbFz1Arr1 = C12()
    Case 13: VbFz1Arr1 = C13()
    Case 14: VbFz1Arr1 = C14()
    Case 15: VbFz1Arr1 = C15()
    Case 16: VbFz1Arr1 = C16()
    Case 17: VbFz1Arr1 = C17()
    Case 18: VbFz1Arr1 = C18()
    Case 19: VbFz1Arr1 = C19()
    Case 20: VbFz1Arr1 = C20()
    Case 21: VbFz1Arr1 = C21()
    Case 22: VbFz1Arr1 = C22()
    Case 23: VbFz1Arr1 = C23()
    Case 24: VbFz1Arr1 = C24()
    Case 25: VbFz1Arr1 = C25()
    Case 26: VbFz1Arr1 = C26()
    Case 27: VbFz1Arr1 = C27()
    Case 28: VbFz1Arr1 = C28()
    Case 29: VbFz1Arr1 = C29()
    Case 30: VbFz1Arr1 = C30()
    Case 31: VbFz1Arr1 = C31()
    Case 32: VbFz1Arr1 = C32()
    Case 33: VbFz1Arr1 = C33()
    Case 34: VbFz1Arr1 = C34()
    Case 35: VbFz1Arr1 = C35()
    Case 36: VbFz1Arr1 = C36()
    Case 37: VbFz1Arr1 = C37()
    Case 38: VbFz1Arr1 = C38()
    Case 39: VbFz1Arr1 = C39()
    Case 40: VbFz1Arr1 = C40()
    Case 41: VbFz1Arr1 = C41()
    Case 42: VbFz1Arr1 = C42()
    Case 43: VbFz1Arr1 = C43()
    Case 44: VbFz1Arr1 = C44()
    Case 45: VbFz1Arr1 = C45()
    Case 46: VbFz1Arr1 = C46()
    Case 47: VbFz1Arr1 = C47()
    Case 48: VbFz1Arr1 = C48()
    Case 49: VbFz1Arr1 = C49()
    Case 50: VbFz1Arr1 = C50()
    Case 51: VbFz1Arr1 = C51()
    Case 52: VbFz1Arr1 = C52()
    Case 53: VbFz1Arr1 = C53()
    Case 54: VbFz1Arr1 = C54()
    Case 55: VbFz1Arr1 = C55()
    Case 56: VbFz1Arr1 = C56()
    Case 57: VbFz1Arr1 = C57()
    Case 58: VbFz1Arr1 = C58()
    Case 59: VbFz1Arr1 = C59()
    Case 60: VbFz1Arr1 = C60()
    Case 61: VbFz1Arr1 = C61()
    Case 62: VbFz1Arr1 = C62()
    Case 63: VbFz1Arr1 = C63()
    Case 64: VbFz1Arr1 = C64()
    Case 65: VbFz1Arr1 = C65()
    Case 66: VbFz1Arr1 = C66()
    Case 67: VbFz1Arr1 = C67()
    Case 68: VbFz1Arr1 = C68()
    Case 69: VbFz1Arr1 = C69()
    Case 70: VbFz1Arr1 = C70()
    Case 71: VbFz1Arr1 = C71()
    Case 72: VbFz1Arr1 = C72()
    Case 73: VbFz1Arr1 = C73()
    Case 74: VbFz1Arr1 = C74()
    Case 75: VbFz1Arr1 = C75()
    Case 76: VbFz1Arr1 = C76()
    Case 77: VbFz1Arr1 = C77()
    Case 78: VbFz1Arr1 = C78()
    Case 79: VbFz1Arr1 = C79()
    Case 80: VbFz1Arr1 = C80()
    Case 81: VbFz1Arr1 = C81()
    Case 82: VbFz1Arr1 = C82()
    Case 83: VbFz1Arr1 = C83()
    Case 84: VbFz1Arr1 = C84()
    Case 85: VbFz1Arr1 = C85()
    Case 86: VbFz1Arr1 = C86()
    Case 87: VbFz1Arr1 = C87()
    Case 88: VbFz1Arr1 = C88()
    Case 89: VbFz1Arr1 = C89()
    Case 90: VbFz1Arr1 = C90()
    Case 91: VbFz1Arr1 = C91()
    Case 92: VbFz1Arr1 = C92()
    Case 93: VbFz1Arr1 = C93()
    Case 94: VbFz1Arr1 = C94()
    Case 95: VbFz1Arr1 = C95()
    Case 96: VbFz1Arr1 = C96()
    Case 97: VbFz1Arr1 = C97()
    Case 98: VbFz1Arr1 = C98()
    Case 99: VbFz1Arr1 = C99()
    Case 100: VbFz1Arr1 = C100()
    Case 101: VbFz1Arr1 = C101()
    Case 102: VbFz1Arr1 = C102()
    Case 103: VbFz1Arr1 = C103()
    Case 104: VbFz1Arr1 = C104()
    Case 105: VbFz1Arr1 = C105()
    Case 106: VbFz1Arr1 = C106()
    Case 107: VbFz1Arr1 = C107()
    Case 108: VbFz1Arr1 = C108()
    Case 109: VbFz1Arr1 = C109()
    Case 110: VbFz1Arr1 = C110()
    Case 111: VbFz1Arr1 = C111()
    Case 112: VbFz1Arr1 = C112()
    Case 113: VbFz1Arr1 = C113()
    Case 114: VbFz1Arr1 = C114()
    Case 115: VbFz1Arr1 = C115()
    Case 116: VbFz1Arr1 = C116()
    Case 117: VbFz1Arr1 = C117()
    Case 118: VbFz1Arr1 = C118()
    Case 119: VbFz1Arr1 = C119()
    Case 120: VbFz1Arr1 = C120()
    Case 121: VbFz1Arr1 = C121()
    Case 122: VbFz1Arr1 = C122()
    Case 123: VbFz1Arr1 = C123()
    Case 124: VbFz1Arr1 = C124()
    Case 125: VbFz1Arr1 = C125()
    Case 126: VbFz1Arr1 = C126()
    Case 127: VbFz1Arr1 = C127()
    Case 128: VbFz1Arr1 = C128()
    Case 129: VbFz1Arr1 = C129()
    Case 130: VbFz1Arr1 = C130()
    Case 131: VbFz1Arr1 = C131()
    Case 132: VbFz1Arr1 = C132()
    Case 133: VbFz1Arr1 = C133()
    Case 134: VbFz1Arr1 = C134()
    Case 135: VbFz1Arr1 = C135()
    Case 136: VbFz1Arr1 = C136()
    Case 137: VbFz1Arr1 = C137()
    Case 138: VbFz1Arr1 = C138()
    Case 139: VbFz1Arr1 = C139()
    Case 140: VbFz1Arr1 = C140()
    Case 141: VbFz1Arr1 = C141()
    Case 142: VbFz1Arr1 = C142()
    Case 143: VbFz1Arr1 = C143()
    Case 144: VbFz1Arr1 = C144()
    Case 145: VbFz1Arr1 = C145()
    Case 146: VbFz1Arr1 = C146()
    Case 147: VbFz1Arr1 = C147()
    Case 148: VbFz1Arr1 = C148()
    Case 149: VbFz1Arr1 = C149()
    End Select
    Exit Function
EH:
    VbFz1Arr1 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = UBound(w, 1): s = s & Ap(v)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(1 To 5): s = s & Ap(w)
    Err.Clear: v = Empty: v = UBound(g, 1): s = s & Ap(v)
    Err.Clear: w = Array(2, 5, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(0 To 0): s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 5: i = 1: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(0 To 4): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: Erase f: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim Preserve a(1 To 6): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: v = Empty: v = UBound(f, 1): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 7: i = 0: f(i) = 1: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(5, 3, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(-2 To -2): s = s & Ap(a)
    For i = -2 To -2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Array(5, 1, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(-2, -1): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(1 To 5): s = s & Ap(w)
    Err.Clear: m = MkL(-2, 2): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: v = Empty: v = UBound(g, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = LBound(a, 2): s = s & Ap(v)
    Err.Clear: m = MkL(-2, 1): s = s & Ap(m): ModL m, -1: s = s & Ap(m)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: m = MkL(-2, 3): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(w, 3): s = s & Ap(v)
    Err.Clear: ReDim a(5 To 5): s = s & Ap(a)
    For i = 5 To 5: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 1): s = s & Ap(a)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: m = MkL(-1, 1): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 2: i = 0: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim Preserve a(0 To 1): s = s & Ap(a)
    Err.Clear: w = Array(1, 2): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: m = MkL(-1, -1): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(2, 4, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: i = 2: f(i) = 1: i = 1: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(3 To 4): s = s & Ap(w)
    Err.Clear: m = MkL(-2, 1): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(5 To 7): s = s & Ap(a)
    For i = 5 To 7: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(5 To 7): s = s & Ap(a)
    For i = 5 To 7: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: Erase g: s = s & Ap(f)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase f: s = s & Ap(f)
    Err.Clear: m = MkL(-2, 4): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(0 To 0): s = s & Ap(a)
    For i = 0 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim Preserve a(1 To -1): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(1, 1, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 4, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: v = Empty: v = LBound(w, 0): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 8: i = 0: f(i) = 9: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(1 To 1): s = s & Ap(w)
    Err.Clear: ReDim a(5 To 7): s = s & Ap(a)
    For i = 5 To 7: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(a, 3): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(5 To 2): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 3): s = s & Ap(a)
    Err.Clear: w = Array(4, 2, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: v = Empty: v = LBound(a, 2): s = s & Ap(v)
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 4): s = s & Ap(a)
    For i = 1 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(2 To 1): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 2): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(1 To -1): s = s & Ap(a)
    Err.Clear: Erase g: s = s & Ap(g)
    Err.Clear: w = Split("x", ","): s = s & Ap(w): ReDim Preserve w(2 To 2): s = s & Ap(w)
    Err.Clear: ReDim Preserve a(1 To 5): s = s & Ap(a)
    Err.Clear: ReDim a(0 To 1): s = s & Ap(a)
    For i = 0 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: Erase g: s = s & Ap(f)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: v = Empty: v = UBound(a, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = LBound(w, 1): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 4: i = 1: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(3 To 1): s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(w, 1): s = s & Ap(v)
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(-1 To 2): s = s & Ap(a)
    For i = -1 To 2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(1 To 2): s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(f, 2): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase g: s = s & Ap(a)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(2 To 3): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(-2 To -3): s = s & Ap(a)
    For i = -2 To -3: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: m = MkL(2, 0): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(2 To 3): s = s & Ap(w)
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: ReDim a(0 To 4): s = s & Ap(a)
    For i = 0 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(2 To 1): s = s & Ap(a)
    Err.Clear: m = MkL(-2, -1): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 4: i = 1: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(3, 3, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(2, 4, True): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(4, 2, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(1, 0): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: ReDim Preserve a(1 To 5): s = s & Ap(a)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(1 To 5): s = s & Ap(w)
    Err.Clear: ReDim Preserve a(0 To 4): s = s & Ap(a)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(5, 1, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 3): s = s & Ap(a)
    For i = 1 To 3: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: v = Empty: v = UBound(a, 0): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 6: i = 0: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(4, 5, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(3, 4, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(-1, 1): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: ReDim Preserve a(5 To 1): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(0 To 3): s = s & Ap(a)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(-1, 3): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 8: i = 1: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: v = Empty: v = LBound(a, 0): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split("x", ","): s = s & Ap(w): ReDim Preserve w(3 To 2): s = s & Ap(w)
    Err.Clear: ReDim Preserve a(1 To 3): s = s & Ap(a)
    Err.Clear: m = MkL(-1, -1): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(f, 3): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 6: i = 0: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase g: s = s & Ap(g)
    Err.Clear: v = Empty: v = LBound(g, 1): s = s & Ap(v)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(0 To 5): s = s & Ap(w)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(1 To 3): s = s & Ap(w)
    Err.Clear: ReDim a(1 To 3): s = s & Ap(a)
    For i = 1 To 3: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 3): s = s & Ap(a)
    For i = 1 To 3: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 5: i = 1: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: i = 2: f(i) = 4: i = 0: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: v = Empty: v = LBound(g, 0): s = s & Ap(v)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(f, 1): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(3, 3, True): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: m = MkL(-2, -1): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: i = 2: f(i) = 7: i = 1: f(i) = 3: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: Erase f: s = s & Ap(a)
    Err.Clear: v = Empty: v = LBound(a, 0): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase f: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: Erase f: s = s & Ap(a)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(0 To 1): s = s & Ap(w)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 8: i = 1: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(4, 1, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(2, 2, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: ReDim a(-2 To -3): s = s & Ap(a)
    For i = -2 To -3: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = LBound(a, 3): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 2): s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(w, 0): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(0 To 3): s = s & Ap(w)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(3 To 1): s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(-1 To 1): s = s & Ap(a)
    For i = -1 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(g, 3): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(-1 To 1): s = s & Ap(a)
    For i = -1 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 7: i = 0: f(i) = 1: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: m = MkL(2, 4): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(2 To 2): s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(g, 3): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: Erase f: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(2 To 4): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 8: i = 0: f(i) = 3: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: Erase f: s = s & Ap(f)
    Err.Clear: v = Empty: v = UBound(f, 3): s = s & Ap(v)
    Err.Clear: m = MkL(-2, 2): s = s & Ap(m): ModL m, -1: s = s & Ap(m)
    Err.Clear: w = Array(4, 1, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(-2 To -3): s = s & Ap(a)
    For i = -2 To -3: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(-2 To 5): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(0 To 1): s = s & Ap(w)
    Err.Clear: w = Array(2, 5, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(w, 0): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 2: i = 1: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 2: i = 1: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 4: i = 0: f(i) = 7: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(5 To 5): s = s & Ap(a)
    For i = 5 To 5: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(2 To 1): s = s & Ap(w)
    Err.Clear: w = Array(3, 5, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(0 To 2): s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(g, 1): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(0 To 2): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(2 To 3): s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 9: i = 0: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = LBound(f, 2): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 6: i = 1: f(i) = 7: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: m = MkL(-2, 4): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(2, 2): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: ReDim a(-2 To -3): s = s & Ap(a)
    For i = -2 To -3: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 2): s = s & Ap(a)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 1): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim Preserve a(0 To 4): s = s & Ap(a)
    Err.Clear: m = MkL(1, 0): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: Erase g: s = s & Ap(f)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(0 To 0): s = s & Ap(a)
    For i = 0 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: v = Empty: v = LBound(a, 2): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 3: i = 1: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(0 To 3): s = s & Ap(a)
    Err.Clear: w = Array(1, 4, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(0, 2): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: v = Empty: v = UBound(g, 2): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 1: i = 0: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim Preserve a(0 To 1): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(-1 To -2): s = s & Ap(a)
    For i = -1 To -2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: Erase f: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(1 To 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = LBound(w, 2): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(-1 To 0): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = LBound(w, 0): s = s & Ap(v)
    Err.Clear: w = Array(4, 4, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim Preserve a(5 To 4): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(0 To 2): s = s & Ap(a)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(1 To 5): s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(1, 3): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    Err.Clear: w = Array(3, 4, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 4: i = 1: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(1 To 2): s = s & Ap(w)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(1, 3): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(1 To 1): s = s & Ap(w)
    Err.Clear: m = MkL(-1, 4): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(-2 To 2): s = s & Ap(a)
    For i = -2 To 2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 1: i = 1: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: i = 2: f(i) = 3: i = 0: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(2 To 4): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(1 To -1): s = s & Ap(a)
    Err.Clear: w = Array(2, 1, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(0, -1): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: i = 2: f(i) = 2: i = 1: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(-1 To 1): s = s & Ap(a)
    For i = -1 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(-1 To 1): s = s & Ap(a)
    For i = -1 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Array(2, 1, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(1, 0): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(2 To 4): s = s & Ap(w)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: Erase g: s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 8: i = 0: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim Preserve a(1 To 2): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(w, 2): s = s & Ap(v)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = UBound(w, 2): s = s & Ap(v)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 5: i = 1: f(i) = 3: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 4, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(2 To 2): s = s & Ap(w)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: Erase g: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: m = MkL(1, 0): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(1 To 3): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 2): s = s & Ap(a)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(3 To 4): s = s & Ap(w)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(3 To 4): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim Preserve a(1 To 6): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(1, 2): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase f: s = s & Ap(g)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(0 To 4): s = s & Ap(w)
    Err.Clear: m = MkL(1, 4): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 3: i = 1: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(a, 1): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 3): s = s & Ap(a)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(1 To 2): s = s & Ap(a)
    Err.Clear: w = Array(1, 2): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(5 To 5): s = s & Ap(a)
    For i = 5 To 5: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(3 To 2): s = s & Ap(w)
    Err.Clear: ReDim a(1 To 4): s = s & Ap(a)
    For i = 1 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(2 To 3): s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: Erase f: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 4): s = s & Ap(a)
    For i = 1 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: Erase f: s = s & Ap(f)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(1, 4, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(3 To 2): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(3, 2, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: i = 2: f(i) = 6: i = 1: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: m = MkL(1, 3): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(5, 3, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(5, 5, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(-2, 3): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: w = Array(3, 2, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(2, 4, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: m = MkL(1, 0): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 9: i = 1: f(i) = 1: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(0 To -1): s = s & Ap(a)
    For i = 0 To -1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(0 To 2): s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(w, 1): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(1, 0): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 4): s = s & Ap(a)
    For i = 1 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: m = MkL(-2, 4): s = s & Ap(m): ModL m, -1: s = s & Ap(m)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 4): s = s & Ap(a)
    For i = 1 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(w, 2): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 6: i = 1: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: m = MkL(1, 3): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(1, 4, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(2 To 4): s = s & Ap(w)
    Err.Clear: ReDim a(1 To 3): s = s & Ap(a)
    For i = 1 To 3: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: ReDim a(-1 To 3): s = s & Ap(a)
    For i = -1 To 3: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Array(3, 3, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim Preserve a(2 To 2): s = s & Ap(a)
    Err.Clear: m = MkL(-2, 0): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(g, 0): s = s & Ap(v)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 3: i = 0: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(-1, 2): s = s & Ap(m): ModL m, -1: s = s & Ap(m)
    Err.Clear: ReDim Preserve a(2 To 1): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim Preserve a(1 To 5): s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 9: i = 1: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 5, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(0, 3): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: v = Empty: v = UBound(g, 1): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 1: i = 1: f(i) = 3: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(3 To 5): s = s & Ap(w)
    Err.Clear: Erase g: s = s & Ap(g)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(5 To 8): s = s & Ap(a)
    For i = 5 To 8: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 6: i = 1: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 4: i = 0: f(i) = 1: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(5, 2, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: Erase f: s = s & Ap(g)
    Err.Clear: Erase f: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(0 To 3): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: m = MkL(-2, 3): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(4, 2, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = LBound(f, 2): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: m = MkL(2, 1): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: m = MkL(1, 1): s = s & Ap(m): ModL m, -1: s = s & Ap(m)
    Err.Clear: ReDim Preserve a(2 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(a, 1): s = s & Ap(v)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 7: i = 1: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 1): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(5 To 6): s = s & Ap(a)
    For i = 5 To 6: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(g, 0): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 6: i = 1: f(i) = 9: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: m = MkL(1, 2): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(5, 4, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(2, 1): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    Err.Clear: v = Empty: v = LBound(a, 1): s = s & Ap(v)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 3): s = s & Ap(a)
    Err.Clear: v = Empty: v = LBound(f, 0): s = s & Ap(v)
    Err.Clear: Erase g: s = s & Ap(f)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(-2, -1): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: v = Empty: v = UBound(g, 3): s = s & Ap(v)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase a: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: v = Empty: v = LBound(w, 3): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(-2 To -1): s = s & Ap(a)
    For i = -2 To -1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: Erase f: s = s & Ap(g)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(1 To 1): s = s & Ap(w)
    Err.Clear: ReDim Preserve a(2 To -1): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 1): s = s & Ap(a)
    Err.Clear: w = Array(1, 4, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(1, 1): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: w = Array(1, 5, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2): s = s & Ap(a)
    For i = 1 To 2: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(2, 1, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(2, 1): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: w = Array(4, 2, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(2 To 1): s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 7: i = 0: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(-1 To -2): s = s & Ap(a)
    For i = -1 To -2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(w, 2): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(1 To 5): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(g, 2): s = s & Ap(v)
    Err.Clear: w = Array(5, 5, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: m = MkL(-2, 3): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: ReDim a(5 To 6): s = s & Ap(a)
    For i = 5 To 6: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Split("x", ","): s = s & Ap(w): ReDim Preserve w(0 To 5): s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: m = MkL(0, 4): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: m = MkL(0, 0): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: v = Empty: v = LBound(w, 0): s = s & Ap(v)
    Err.Clear: Erase f: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase f: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(1, 2): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 1): s = s & Ap(a)
    For i = 1 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(-2, 4): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim Preserve a(1 To 2): s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 7: i = 1: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(3 To 1): s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(a, 3): s = s & Ap(v)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(3 To 3): s = s & Ap(w)
    Err.Clear: Erase f: s = s & Ap(f)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 4: i = 0: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 1, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: Erase f: s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 3: i = 0: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(5, 3, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(-2 To 0): s = s & Ap(a)
    Err.Clear: w = Array(3, 5, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: Erase f: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(0 To 4): s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(a, 0): s = s & Ap(v)
    Err.Clear: m = MkL(2, 4): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: ReDim a(-1 To -2): s = s & Ap(a)
    For i = -1 To -2: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = LBound(f, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = LBound(a, 2): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: m = MkL(2, 1): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: m = MkL(1, 0): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(-1 To 0): s = s & Ap(a)
    For i = -1 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 3): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(-1 To 4): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: i = 2: f(i) = 3: i = 0: f(i) = 9: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 2): s = s & Ap(a)
    Err.Clear: w = Array(3, 3, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(0 To 4): s = s & Ap(a)
    For i = 0 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(2, 2, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 8: i = 0: f(i) = 1: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: m = MkL(-1, 2): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 1): s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 6: i = 0: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim Preserve a(2 To -1): s = s & Ap(a)
    Err.Clear: Erase g: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(-2 To 2): s = s & Ap(a)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(2 To 4): s = s & Ap(w)
    Err.Clear: m = MkL(-1, 1): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: w = Split("x", ","): s = s & Ap(w): ReDim Preserve w(0 To 5): s = s & Ap(w)
    Err.Clear: ReDim Preserve a(2 To 6): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(2 To 1): s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Split("x", ","): s = s & Ap(w): ReDim Preserve w(2 To 4): s = s & Ap(w)
    Err.Clear: ReDim a(5 To 6): s = s & Ap(a)
    For i = 5 To 6: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(1 To -1): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 7: i = 0: f(i) = 9: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: v = Empty: v = LBound(a, 0): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(1 To 2): s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(g, 0): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: m = MkL(0, 0): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(g, 0): s = s & Ap(v)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(2 To 5): s = s & Ap(w)
    Err.Clear: w = Array(2, 4, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 4: i = 0: f(i) = 1: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(2 To 1): s = s & Ap(a)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(3 To 3): s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 2: i = 0: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: Erase g: s = s & Ap(f)
    Err.Clear: i = 2: f(i) = 2: i = 1: f(i) = 3: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = UBound(f, 0): s = s & Ap(v)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(2 To 5): s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(w, 0): s = s & Ap(v)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(1 To 1): s = s & Ap(w)
    Err.Clear: v = Empty: v = UBound(f, 2): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(0 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 7: i = 1: f(i) = 8: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(g, 2): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 2: i = 0: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: Erase g: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase a: s = s & Ap(f)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(3 To 3): s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: w = Array(1, 2): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(0 To 3): s = s & Ap(w)
    Err.Clear: v = Empty: v = UBound(f, 0): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase f: s = s & Ap(a)
    Err.Clear: Erase g: s = s & Ap(g)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(5 To 8): s = s & Ap(a)
    For i = 5 To 8: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 1: i = 0: f(i) = 7: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: v = Empty: v = LBound(g, 3): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(0, 3): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 3): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: Erase a: s = s & Ap(f)
    Err.Clear: v = Empty: v = LBound(a, 3): s = s & Ap(v)
    Err.Clear: w = Split("x", ","): s = s & Ap(w): ReDim Preserve w(0 To 4): s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(1 To 1): s = s & Ap(w)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 3): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(0 To 4): s = s & Ap(a)
    Err.Clear: m = MkL(0, 1): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2): s = s & Ap(a)
    For i = 1 To 2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Array(1, 3, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: Erase g: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(-1, 4): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: w = Split("x", ","): s = s & Ap(w): ReDim Preserve w(0 To 3): s = s & Ap(w)
    Err.Clear: m = MkL(-2, -1): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim Preserve a(0 To 5): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(1 To 0): s = s & Ap(a)
    Err.Clear: m = MkL(0, 2): s = s & Ap(m): ModL m, -1: s = s & Ap(m)
    Err.Clear: ReDim Preserve a(1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(0 To 3): s = s & Ap(a)
    For i = 0 To 3: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(0 To 6): s = s & Ap(a)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(0 To 1): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array(1, 2): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(5 To 6): s = s & Ap(a)
    For i = 5 To 6: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(0 To 1): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = LBound(a, 0): s = s & Ap(v)
    Err.Clear: w = Array(1, 3, True): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: v = Empty: v = UBound(f, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = UBound(f, 3): s = s & Ap(v)
    Err.Clear: Erase g: s = s & Ap(f)
    Err.Clear: w = Array(2, 2, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 1: i = 0: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim Preserve a(2 To 3): s = s & Ap(a)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(3 To 1): s = s & Ap(w)
    Err.Clear: w = Array(5, 5, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(5 To 4): s = s & Ap(a)
    For i = 5 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(5 To 4): s = s & Ap(a)
    For i = 5 To 4: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(1 To -1): s = s & Ap(a)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 2): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Split("x", ","): s = s & Ap(w): ReDim Preserve w(0 To 4): s = s & Ap(w)
    Err.Clear: w = Array(3, 1, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Currency, b() As Currency, f(2) As Currency, g(1 To 2, 0 To 1) As Currency, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 8: i = 0: f(i) = 7: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(2 To 3): s = s & Ap(w)
    Err.Clear: Erase f: s = s & Ap(f)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(0, -1): s = s & Ap(m): ModL m, 0: s = s & Ap(m)
    Err.Clear: m = MkL(1, 1): s = s & Ap(m): ModL m, -1: s = s & Ap(m)
    Err.Clear: w = Array(4, 5, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: Erase a: s = s & Ap(a)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(2 To 5): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(0 To 1): s = s & Ap(a)
    For i = 0 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: v = Empty: v = UBound(f, 3): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(3 To 1): s = s & Ap(w)
    Err.Clear: ReDim a(0 To 1): s = s & Ap(a)
    For i = 0 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(0 To -1): s = s & Ap(a)
    For i = 0 To -1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(2 To 4): s = s & Ap(w)
    Err.Clear: Erase a: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(0 To 4): s = s & Ap(a)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(2 To 5): s = s & Ap(w)
    Err.Clear: Erase a: s = s & Ap(f)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(1 To 1): s = s & Ap(a)
    Err.Clear: Erase a: s = s & Ap(f)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: w = Array(4, 5, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: Erase g: s = s & Ap(f)
    Err.Clear: i = 2: f(i) = 8: i = 0: f(i) = 7: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(5, 2, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 1): s = s & Ap(a)
    Err.Clear: w = Array(2, 5, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: Erase f: s = s & Ap(g)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(2, 1, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: i = 2: f(i) = 9: i = 1: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: Erase a: s = s & Ap(a)
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: v = Empty: v = LBound(f, 2): s = s & Ap(v)
    Err.Clear: w = Array(4, 4, "3"): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 3: i = 0: f(i) = 9: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: v = Empty: v = LBound(f, 3): s = s & Ap(v)
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim Preserve a(1 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(2 To 4): s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 3): s = s & Ap(a)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(0): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim Preserve a(0 To 6): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(2 To 0): s = s & Ap(a)
    Err.Clear: ReDim a(-2 To 0): s = s & Ap(a)
    For i = -2 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(1 To -1): s = s & Ap(a)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2): s = s & Ap(a)
    For i = 1 To 2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(0 To 3): s = s & Ap(w)
    Err.Clear: Erase f: s = s & Ap(f)
    Err.Clear: w = Array(2, 2, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim a(1 To 2): s = s & Ap(a)
    For i = 1 To 2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(-2 To 1): s = s & Ap(a)
    For i = -2 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim Preserve a(1 To 2): s = s & Ap(a)
    Err.Clear: m = MkL(0, 0): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(1 To 2): s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: ReDim Preserve a(1 To 1): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase a: s = s & Ap(f)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: m = MkL(-2, 4): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: ReDim a(1): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(3 To 5): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: Erase a: s = s & Ap(f)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(3, 4, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: v = Empty: v = LBound(w, 2): s = s & Ap(v)
    Err.Clear: w = Array(2, 4, True): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(5, 2, CCur(1)): v = Empty: v = SumV(w): s = s & Ap(v): i = -1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim Preserve a(2 To 5): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(2 To 4): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 1): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(1 To 3): s = s & Ap(w)
    Err.Clear: ReDim a(-2 To -1): s = s & Ap(a)
    For i = -2 To -1: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(1, 1): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim Preserve a(-1 To 4): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 2): s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: v = Empty: v = LBound(g, 1): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(3, 3, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: m = MkL(-1, 4): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 3): s = s & Ap(a)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: ReDim a(0 To 3): s = s & Ap(a)
    For i = 0 To 3: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(1 To 5): s = s & Ap(w)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(0 To 4): s = s & Ap(w)
    Err.Clear: ReDim Preserve a(2 To 5): s = s & Ap(a)
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Variant, b() As Variant, f(2) As Variant, g(1 To 2, 0 To 1) As Variant, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(2, 3): s = s & Ap(m): ModL m, 3: s = s & Ap(m)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(3 To 3): s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(f, 0): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 0): s = s & Ap(a)
    For i = 1 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 0): s = s & Ap(a)
    For i = 1 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: Erase a: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(4, 1, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 2: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim Preserve a(1 To 2): s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 6: i = 0: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(0 To 1): s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 9: i = 1: f(i) = 9: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: v = Empty: v = LBound(g, 0): s = s & Ap(v)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 0): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(0 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: Erase f: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Double, b() As Double, f(2) As Double, g(1 To 2, 0 To 1) As Double, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(4, 2, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 3: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: ReDim Preserve a(1 To -1): s = s & Ap(a)
    Err.Clear: Erase g: s = s & Ap(a)
    Err.Clear: w = Array(1, 2, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: Erase a: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 0 To 1): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: ReDim a(-1 To -2): s = s & Ap(a)
    For i = -1 To -2: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 5: i = 1: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: ReDim a(1 To 2, 1 To 2): ReDim Preserve a(1 To 3, 1 To 3): s = s & Ap(a)
    Err.Clear: m = MkL(2, -1): s = s & Ap(m): ModL m, -2: s = s & Ap(m)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: Erase a: s = s & Ap(g)
    Err.Clear: w = Split("a,,b", ","): s = s & Ap(w): ReDim Preserve w(3 To 5): s = s & Ap(w)
    Err.Clear: w = Array(1): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: i = 2: f(i) = 5: i = 1: f(i) = 9: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: v = Empty: v = LBound(g, 0): s = s & Ap(v)
    Err.Clear: w = Split("", ","): s = s & Ap(w): ReDim Preserve w(2 To 1): s = s & Ap(w)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(0 To 2): s = s & Ap(w)
    Err.Clear: ReDim a(5 To 6): s = s & Ap(a)
    For i = 5 To 6: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(1 To 2, 0 To 1): For i = 1 To 2: For j = 0 To 1: a(i, j) = i * 10 + j: Next j: Next i: ReDim Preserve a(1 To 2, 1 To 2): s = s & Ap(a)
    Err.Clear: For i = 1 To 2: For j = 0 To 1: g(i, j) = i * 10 + j: Next j: Next i: s = s & Ap(g)
    Err.Clear: i = 2: f(i) = 6: i = 1: f(i) = 9: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: ReDim Preserve a(2 To 3): s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Integer, b() As Integer, f(2) As Integer, g(1 To 2, 0 To 1) As Integer, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: ReDim a(3): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: m = MkL(-1, 1): s = s & Ap(m): ModL m, -1: s = s & Ap(m)
    Err.Clear: w = Split("a,b,c", ","): s = s & Ap(w): ReDim Preserve w(1 To 1): s = s & Ap(w)
    Err.Clear: Erase f: s = s & Ap(f)
    Err.Clear: ReDim a(-1 To 1): s = s & Ap(a)
    For i = -1 To 1: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Boolean, b() As Boolean, f(2) As Boolean, g(1 To 2, 0 To 1) As Boolean, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(Empty, Null): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: i = 2: f(i) = 9: i = 0: f(i) = 6: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: i = 2: f(i) = 8: i = 1: f(i) = 4: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As String, b() As String, f(2) As String, g(1 To 2, 0 To 1) As String, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(2, 2): s = s & Ap(m): ModL m, 2: s = s & Ap(m)
    Err.Clear: w = Split(",", ","): s = s & Ap(w): ReDim Preserve w(1 To 1): s = s & Ap(w)
    Err.Clear: Erase g: s = s & Ap(f)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: w = Array("a", 2.5, True): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(5, 3, 2.5): v = Empty: v = SumV(w): s = s & Ap(v): i = 1: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Byte, b() As Byte, f(2) As Byte, g(1 To 2, 0 To 1) As Byte, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: w = Array(): s = s & Ap(w): v = Empty: v = LBound(w): s = s & Ap(v)
    Err.Clear: w = Array(1, 2, 3): ModV w: s = s & Ap(w): ModVR w: s = s & Ap(w)
    Err.Clear: v = Empty: v = LBound(a, 2): s = s & Ap(v)
    Err.Clear: ReDim a(2): s = s & Ap(a): v = Empty: v = LBound(a) & ":" & UBound(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = LBound(g, 0): s = s & Ap(v)
    Err.Clear: ReDim a(-2 To 0): s = s & Ap(a)
    For i = -2 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a() As Long, b() As Long, f(2) As Long, g(1 To 2, 0 To 1) As Long, m() As Long, w As Variant, i As Long, j As Long
    Err.Clear: m = MkL(-1, 3): s = s & Ap(m): ModL m, 1: s = s & Ap(m)
    Err.Clear: ReDim a(-1 To 0): s = s & Ap(a)
    For i = -1 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: ReDim a(-1 To 0): s = s & Ap(a)
    For i = -1 To 0: a(i) = i * 2: Next i: s = s & Ap(a)
    Err.Clear: i = 2: f(i) = 7: i = 0: f(i) = 2: b = f: s = s & Ap(b): b(i) = 77: s = s & Ap(f) & Ap(b)
    Err.Clear: v = Empty: v = UBound(f, 1): s = s & Ap(v)
    Err.Clear: w = Array(1, 1, True): v = Empty: v = SumV(w): s = s & Ap(v): i = 0: v = w(i): s = s & Ap(v) & Ap(f(i))
    s = s & IsArray(a) & VarType(a) & TypeName(f) & VarType(w)
    C149 = s
End Function
