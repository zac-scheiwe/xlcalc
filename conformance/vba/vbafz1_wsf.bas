Attribute VB_Name = "ModVbFz1Wsf"
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
Public Function VbFz1Wsf(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1Wsf = C0()
    Case 1: VbFz1Wsf = C1()
    Case 2: VbFz1Wsf = C2()
    Case 3: VbFz1Wsf = C3()
    Case 4: VbFz1Wsf = C4()
    Case 5: VbFz1Wsf = C5()
    Case 6: VbFz1Wsf = C6()
    Case 7: VbFz1Wsf = C7()
    Case 8: VbFz1Wsf = C8()
    Case 9: VbFz1Wsf = C9()
    Case 10: VbFz1Wsf = C10()
    Case 11: VbFz1Wsf = C11()
    Case 12: VbFz1Wsf = C12()
    Case 13: VbFz1Wsf = C13()
    Case 14: VbFz1Wsf = C14()
    Case 15: VbFz1Wsf = C15()
    Case 16: VbFz1Wsf = C16()
    Case 17: VbFz1Wsf = C17()
    Case 18: VbFz1Wsf = C18()
    Case 19: VbFz1Wsf = C19()
    Case 20: VbFz1Wsf = C20()
    Case 21: VbFz1Wsf = C21()
    Case 22: VbFz1Wsf = C22()
    Case 23: VbFz1Wsf = C23()
    Case 24: VbFz1Wsf = C24()
    Case 25: VbFz1Wsf = C25()
    Case 26: VbFz1Wsf = C26()
    Case 27: VbFz1Wsf = C27()
    Case 28: VbFz1Wsf = C28()
    Case 29: VbFz1Wsf = C29()
    Case 30: VbFz1Wsf = C30()
    Case 31: VbFz1Wsf = C31()
    Case 32: VbFz1Wsf = C32()
    Case 33: VbFz1Wsf = C33()
    Case 34: VbFz1Wsf = C34()
    Case 35: VbFz1Wsf = C35()
    Case 36: VbFz1Wsf = C36()
    Case 37: VbFz1Wsf = C37()
    Case 38: VbFz1Wsf = C38()
    Case 39: VbFz1Wsf = C39()
    Case 40: VbFz1Wsf = C40()
    Case 41: VbFz1Wsf = C41()
    Case 42: VbFz1Wsf = C42()
    Case 43: VbFz1Wsf = C43()
    Case 44: VbFz1Wsf = C44()
    Case 45: VbFz1Wsf = C45()
    Case 46: VbFz1Wsf = C46()
    Case 47: VbFz1Wsf = C47()
    Case 48: VbFz1Wsf = C48()
    Case 49: VbFz1Wsf = C49()
    Case 50: VbFz1Wsf = C50()
    Case 51: VbFz1Wsf = C51()
    Case 52: VbFz1Wsf = C52()
    Case 53: VbFz1Wsf = C53()
    Case 54: VbFz1Wsf = C54()
    Case 55: VbFz1Wsf = C55()
    Case 56: VbFz1Wsf = C56()
    Case 57: VbFz1Wsf = C57()
    Case 58: VbFz1Wsf = C58()
    Case 59: VbFz1Wsf = C59()
    Case 60: VbFz1Wsf = C60()
    Case 61: VbFz1Wsf = C61()
    Case 62: VbFz1Wsf = C62()
    Case 63: VbFz1Wsf = C63()
    Case 64: VbFz1Wsf = C64()
    Case 65: VbFz1Wsf = C65()
    Case 66: VbFz1Wsf = C66()
    Case 67: VbFz1Wsf = C67()
    Case 68: VbFz1Wsf = C68()
    Case 69: VbFz1Wsf = C69()
    Case 70: VbFz1Wsf = C70()
    Case 71: VbFz1Wsf = C71()
    Case 72: VbFz1Wsf = C72()
    Case 73: VbFz1Wsf = C73()
    Case 74: VbFz1Wsf = C74()
    Case 75: VbFz1Wsf = C75()
    Case 76: VbFz1Wsf = C76()
    Case 77: VbFz1Wsf = C77()
    Case 78: VbFz1Wsf = C78()
    Case 79: VbFz1Wsf = C79()
    Case 80: VbFz1Wsf = C80()
    Case 81: VbFz1Wsf = C81()
    Case 82: VbFz1Wsf = C82()
    Case 83: VbFz1Wsf = C83()
    Case 84: VbFz1Wsf = C84()
    Case 85: VbFz1Wsf = C85()
    Case 86: VbFz1Wsf = C86()
    Case 87: VbFz1Wsf = C87()
    Case 88: VbFz1Wsf = C88()
    Case 89: VbFz1Wsf = C89()
    Case 90: VbFz1Wsf = C90()
    Case 91: VbFz1Wsf = C91()
    Case 92: VbFz1Wsf = C92()
    Case 93: VbFz1Wsf = C93()
    Case 94: VbFz1Wsf = C94()
    Case 95: VbFz1Wsf = C95()
    Case 96: VbFz1Wsf = C96()
    Case 97: VbFz1Wsf = C97()
    Case 98: VbFz1Wsf = C98()
    Case 99: VbFz1Wsf = C99()
    Case 100: VbFz1Wsf = C100()
    Case 101: VbFz1Wsf = C101()
    Case 102: VbFz1Wsf = C102()
    Case 103: VbFz1Wsf = C103()
    Case 104: VbFz1Wsf = C104()
    Case 105: VbFz1Wsf = C105()
    Case 106: VbFz1Wsf = C106()
    Case 107: VbFz1Wsf = C107()
    Case 108: VbFz1Wsf = C108()
    Case 109: VbFz1Wsf = C109()
    Case 110: VbFz1Wsf = C110()
    Case 111: VbFz1Wsf = C111()
    Case 112: VbFz1Wsf = C112()
    Case 113: VbFz1Wsf = C113()
    Case 114: VbFz1Wsf = C114()
    Case 115: VbFz1Wsf = C115()
    Case 116: VbFz1Wsf = C116()
    Case 117: VbFz1Wsf = C117()
    Case 118: VbFz1Wsf = C118()
    Case 119: VbFz1Wsf = C119()
    Case 120: VbFz1Wsf = C120()
    Case 121: VbFz1Wsf = C121()
    Case 122: VbFz1Wsf = C122()
    Case 123: VbFz1Wsf = C123()
    Case 124: VbFz1Wsf = C124()
    Case 125: VbFz1Wsf = C125()
    Case 126: VbFz1Wsf = C126()
    Case 127: VbFz1Wsf = C127()
    Case 128: VbFz1Wsf = C128()
    Case 129: VbFz1Wsf = C129()
    Case 130: VbFz1Wsf = C130()
    Case 131: VbFz1Wsf = C131()
    Case 132: VbFz1Wsf = C132()
    Case 133: VbFz1Wsf = C133()
    Case 134: VbFz1Wsf = C134()
    Case 135: VbFz1Wsf = C135()
    Case 136: VbFz1Wsf = C136()
    Case 137: VbFz1Wsf = C137()
    Case 138: VbFz1Wsf = C138()
    Case 139: VbFz1Wsf = C139()
    Case 140: VbFz1Wsf = C140()
    Case 141: VbFz1Wsf = C141()
    Case 142: VbFz1Wsf = C142()
    Case 143: VbFz1Wsf = C143()
    Case 144: VbFz1Wsf = C144()
    Case 145: VbFz1Wsf = C145()
    Case 146: VbFz1Wsf = C146()
    Case 147: VbFz1Wsf = C147()
    Case 148: VbFz1Wsf = C148()
    Case 149: VbFz1Wsf = C149()
    End Select
    Exit Function
EH:
    VbFz1Wsf = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Odd(Range("Sheet1!B4")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.SumIf(CSng(0.1), Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("""abc"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SEQUENCE(2,2)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("x"): s = s & Ap(v) & ErrS()
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("SEQUENCE(2,2)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Quotient(Range("Sheet1!B4"), Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Clean(CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("LEN(C3)"): s = s & Ap(v)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Substitute(CCur(2.25), 0, Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Index(CVErr(2042), CCur(2.25)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult(Array(1, "a", True), "A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("CHAR(65)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("A1:A2 A2:A3"): s = s & Ap(v) & ErrS()
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Upper(Array(1, "a", True)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.VLookup(5, 1E+300, Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm("abc", Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Mod(True, 1E+300): s = s & Ap(v) & ErrS()
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("MAX(B1:B6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max(CDec("1.5"), #1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max("abc", Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("2^0.5"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Sqrt("A1:A3"): s = s & Ap(v) & ErrS()
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Quotient(Range("Sheet1!B1:B6"), CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Min(Range("Sheet1!B4"), CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("""abc"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("CHAR(65)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("=1/0"): s = s & Ap(v)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Power(Range("Sheet1!B1:B6"), Range("Sheet1!A1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Mod(Array(1, 2, 3), Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Bin2Dec("3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Rank_Eq(Range("Sheet1!A1"), "A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("Sheet1!A2"): s = s & Ap(v) & ErrS()
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Ln(0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sign(CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.IsErr(Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("""a""&1.5"): s = s & Ap(v) & ErrS()
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult(5, 2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("CHAR(65)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.CountIf(CLng(3), CCur(2.25)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Percentile("abc", CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product("A1:A3", Array(1, 2, 3)): s = s & Ap(v)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult(1E+300, CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Power(Range("Sheet1!C1"), Range("Sheet1!C1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("1E308*10"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Round(True, #1/2/2000#): s = s & Ap(v)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Small(0, CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Dec2Bin(CDec("1.5")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.MMult(0, Range("Sheet1!A1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.CountIf(CDec("1.5"), Range("Sheet1!C1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("1E308*10"): s = s & Ap(v) & ErrS()
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Weekday(2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Round(True, Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Weekday("A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Small(Range("Sheet1!C1"), Array(1, 2, 3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountIf(Range("Sheet1!B4"), "abc"): s = s & Ap(v) & ErrS()
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Bin2Dec(1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Abs(Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.RoundDown(Range("Sheet1!C1"), CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling(CSng(0.1), Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("x"): s = s & Ap(v)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("ISERROR(C1)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.IsNumber("A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountIf(Range("Sheet1!A1:A6"), Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sqrt(#1/2/2000#): s = s & Ap(v) & ErrS()
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm("abc", CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Bin2Dec("A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product(CLng(3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Floor(1E+300, Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("NA()"): s = s & Ap(v) & ErrS()
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.RoundDown(CVErr(2042), CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Dollar(CCur(2.25), CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Abs(Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Floor("abc", CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Match(#1/2/2000#, Range("Sheet1!A1:A6")): s = s & Ap(v)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Concat(CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Round(CCur(2.25), 0): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Transpose(2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("B1&""x"""): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("""a""&1.5"): s = s & Ap(v)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Round(0, "abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsText(CLng(3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("A1:A3*2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("Sheet1!A2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Rank_Eq(Range("Sheet1!A1:A6"), #1/2/2000#): s = s & Ap(v)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Even(CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Weekday(0, Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Bin2Dec(Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("DATE(2000,1,1)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Abs(#1/2/2000#): s = s & Ap(v) & ErrS()
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.EDate(Empty, CLng(3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountA(Range("Sheet1!C1"), -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Rank_Eq(Range("Sheet1!B4"), -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("A1>A2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Bin2Dec(Range("Sheet1!C1")): s = s & Ap(v)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Product(CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sign(Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sign(Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("ISERROR(C1)"): s = s & Ap(v) & ErrS()
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Asc(CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even("A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Asc("abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.CountA(Range("Sheet1!C1"), True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.N(Range("Sheet1!A1")): s = s & Ap(v)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm(Empty, "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max(5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, "3", True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("2^0.5"): s = s & Ap(v) & ErrS()
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Clean(Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Count(CLng(3), CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor(Array(1, "a", True), 1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("DATE(2000,1,1)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Bin2Dec(CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(1E+300, CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Fact(True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.StDev(CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("10/3"): s = s & Ap(v) & ErrS()
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.N(True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Trim("A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Rept(5, 2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ln(0): s = s & Ap(v)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Quotient(5, Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("A1>A2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Combin(CCur(2.25), Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("0.1+0.2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin(Array(1, 2, 3)): s = s & Ap(v)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.SumIf(Array(1, "a", True), CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("ISERROR(C1)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Index("abc", Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max(-1, 0): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Proper(#1/2/2000#): s = s & Ap(v) & ErrS()
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Index(2.5, "A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.IsErr(CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Days360(Range("Sheet1!C1"), CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Large(Range("Sheet1!C1"), Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product(CVErr(2042), -1): s = s & Ap(v) & ErrS()
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Sqrt(-1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.RoundDown(Range("Sheet1!B1:B6"), Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SEQUENCE(2,2)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(Range("Sheet1!B1:B6"), Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.MMult("A1:A3", CInt(2)): s = s & Ap(v)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Round("3", CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Mod(CInt(2), #1/2/2000#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("1,2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Transpose(Array(1, 2, 3)): s = s & Ap(v) & ErrS()
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(Range("Sheet1!C1"), "abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Combin(CInt(2), "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Match(Range("Sheet1!C1"), CInt(2)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Mod(1E+300, 5): s = s & Ap(v) & ErrS()
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Days360(Range("Sheet1!A1:A6"), Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Small(Array(1, "a", True), CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Floor(5, "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fixed(True, "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("MAX(B1:B6)"): s = s & Ap(v)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("1+1"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Sum(Array(1, 2, 3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("LEN(C3)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Round(Range("Sheet1!A1"), CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Power(Range("Sheet1!A1:A6"), "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("ROUND(2.5,0)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Min(-1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Quotient(Range("Sheet1!A1:A6"), 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Upper(True): s = s & Ap(v) & ErrS()
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("B1&""x"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("NA()"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("LEN(C3)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(2.5, Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.EoMonth(Range("Sheet1!A1"), Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("CHAR(65)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm(2.5, Range("Sheet1!C1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Min(Range("Sheet1!A1"), Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Match(True, Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Rank_Eq(1E+300, Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Clean(Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("=-""3"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("INDEX(A1:A6,3)"): s = s & Ap(v)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Combin(Empty, "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Dollar(5, CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EDate(Range("Sheet1!A1"), CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even(CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Concat(CVErr(2042), CLng(3)): s = s & Ap(v)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor_Math(CCur(2.25), 1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsNumber(CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin("3", 2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Count(Range("Sheet1!B1:B6"), Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Median(5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling(0, True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("=-""3"""): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Sqrt(Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.CountIf(-1, Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("""abc"""): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Mod(Range("Sheet1!A1"), Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.StDev(CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("1+1"): s = s & Ap(v) & ErrS()
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling("3", True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max(Range("Sheet1!A1:A6"), Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.StDev(-1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Dollar(True, 2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Odd(CDec("1.5")): s = s & Ap(v) & ErrS()
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.IsNumber("A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Index("abc", CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Trim(Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.RoundUp("3", CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Dollar(Range("Sheet1!A1:A6"), Array(1, "a", True)): s = s & Ap(v) & ErrS()
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max(2.5, Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product(CInt(2), "abc"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Substitute(CSng(0.1), 1E+300, 0): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor_Math(-1, Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Average(CInt(2)): s = s & Ap(v)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("1+1"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EoMonth(5, Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, CDec("1.5"), Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Floor(1E+300, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.CountIf(CSng(0.1), 5): s = s & Ap(v) & ErrS()
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm(Range("Sheet1!B1:B6"), CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, Array(1, 2, 3), 1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Proper(CDec("1.5")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Weekday(CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.VLookup("abc", CSng(0.1), #1/2/2000#): s = s & Ap(v) & ErrS()
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dollar(CInt(2), Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Ceiling("3", Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("1E308*10"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("ROUND(2.5,0)"): s = s & Ap(v)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Upper(1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Abs(1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Mod(True, 1E+300): s = s & Ap(v) & ErrS()
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EDate(Range("Sheet1!B4"), CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("A1>A2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Text(CCur(2.25), "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Dollar(#1/2/2000#, 1E+300): s = s & Ap(v)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountA(CVErr(2042), "A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("SEQUENCE(2,2)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Median(CInt(2), CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, CDec("1.5"), True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Odd(2.5): s = s & Ap(v) & ErrS()
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(CInt(2), "A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.RoundUp(CVErr(2042), Array(1, "a", True)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.IsText(CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Gcd(Range("Sheet1!B1:B6"), 5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("=-""3"""): s = s & Ap(v) & ErrS()
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin(Range("Sheet1!C1"), Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sum(CInt(2)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EDate(5, CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Concat(5, CCur(2.25)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Substitute(True, "A1:A3", 1E+300): s = s & Ap(v) & ErrS()
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.SumIf(Range("Sheet1!A1:A6"), Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.CountA(#1/2/2000#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ln("abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("1,2"): s = s & Ap(v)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.MMult(CInt(2), True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin(Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Concat("3", "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountA(Array(1, "a", True), CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Count(Array(1, "a", True)): s = s & Ap(v) & ErrS()
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("1,2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm(Empty, 5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Small("abc", Range("Sheet1!B4")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Combin(CSng(0.1), Range("Sheet1!C1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling(CSng(0.1), CLng(3)): s = s & Ap(v) & ErrS()
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Odd(Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, Range("Sheet1!A1"), CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Substitute(CLng(3), "abc", 2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Combin(CCur(2.25), Range("Sheet1!B4")): s = s & Ap(v) & ErrS()
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Small(Array(1, 2, 3), True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Days360(-1, 5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("""a""&1.5"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("=-""3"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.RoundDown(-1, CDec("1.5")): s = s & Ap(v) & ErrS()
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("2^0.5"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Count(Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Round(0, Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Lcm(True, Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Even(Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("=1/0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.CountIf(#1/2/2000#, 1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("MAX(B1:B6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, 1E+300, 1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsErr("A1:A3"): s = s & Ap(v)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product(Range("Sheet1!C1"), "abc"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("ROUND(2.5,0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Dollar(Array(1, 2, 3), CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(Range("Sheet1!C1"), Range("Sheet1!A1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Min(CLng(3), CVErr(2042)): s = s & Ap(v) & ErrS()
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("-2^2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.SumIf(CDec("1.5"), Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Index(-1, Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.IsText(CLng(3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(5): s = s & Ap(v) & ErrS()
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountIf(Range("Sheet1!A1"), "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.IsNumber("abc"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Sum(Range("Sheet1!A1:A6"), Array(1, 2, 3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountIf(Range("Sheet1!C1"), True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin(-1): s = s & Ap(v)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ln(-1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("x"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fixed(1E+300, CDec("1.5")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")"): s = s & Ap(v) & ErrS()
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Substitute(Range("Sheet1!A1:A6"), Range("Sheet1!C1"), Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Transpose("3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sqrt("3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Mod(CVErr(2042), Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product(2.5, "A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Proper(True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Count(#1/2/2000#, Range("Sheet1!B4")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.EoMonth(5, CInt(2)): s = s & Ap(v) & ErrS()
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Round(-1, CLng(3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("ROUND(2.5,0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Count(0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Small(Range("Sheet1!B1:B6"), "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.StDev(-1, Range("Sheet1!A1")): s = s & Ap(v)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("ISERROR(C1)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("""abc"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Combin(Range("Sheet1!B4"), Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Dec2Bin(2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Fact(Range("Sheet1!C1")): s = s & Ap(v)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.StDev(CCur(2.25), 2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin(CCur(2.25), "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.RoundDown(True, True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Large(CDec("1.5"), "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Median(5): s = s & Ap(v) & ErrS()
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult("3", 2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Concat(Range("Sheet1!A1"), CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("=-""3"""): s = s & Ap(v)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Odd("abc"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("B1&""x"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Sum(Array(1, 2, 3), Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Average(CInt(2), 2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(CInt(2)): s = s & Ap(v) & ErrS()
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Upper(2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Match("3", CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Floor(Range("Sheet1!B1:B6"), CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("1+1"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.RoundDown("A1:A3", Range("Sheet1!B4")): s = s & Ap(v)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Upper(CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Dollar(True, Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(CCur(2.25), #1/2/2000#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsText("A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Clean(Range("Sheet1!A1")): s = s & Ap(v)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Floor("3", Array(1, "a", True)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Text(CInt(2), CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sign(-1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Median(CSng(0.1), "abc"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even(2.5): s = s & Ap(v)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Quotient(CVErr(2042), Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm("abc", #1/2/2000#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("INDEX(A1:A6,3)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Power(5, Range("Sheet1!C1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.SumIf("A1:A3", Empty): s = s & Ap(v) & ErrS()
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Match(CSng(0.1), Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Clean(CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Abs(Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Weekday("abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.VLookup(CDec("1.5"), CDec("1.5"), 1E+300): s = s & Ap(v)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Upper(2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, 2.5, "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("=-""3"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("NA()"): s = s & Ap(v) & ErrS()
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("10/3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("=-""3"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("-2^2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Abs(#1/2/2000#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Clean(Range("Sheet1!A1")): s = s & Ap(v) & ErrS()
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Bin2Dec(Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Floor(Range("Sheet1!C1"), 1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("ROUND(2.5,0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("-2^2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max(1E+300, #1/2/2000#): s = s & Ap(v) & ErrS()
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Sign(5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("MAX(B1:B6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Abs(Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("ROUND(2.5,0)"): s = s & Ap(v) & ErrS()
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.VLookup("abc", CSng(0.1), 1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Mod("abc", CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Match(Array(1, "a", True), "A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, Range("Sheet1!A1:A6"), "abc"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v) & ErrS()
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Clean(#1/2/2000#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Match(Range("Sheet1!A1"), 1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Percentile(5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("CHAR(65)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Days360(Array(1, 2, 3), CCur(2.25)): s = s & Ap(v)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountA(CLng(3), Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Mod(Range("Sheet1!B4"), Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("MAX(B1:B6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("10/3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Ln(1E+300): s = s & Ap(v) & ErrS()
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsErr(5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult(True, #1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Substitute(0, CCur(2.25), "abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Trim("abc"): s = s & Ap(v)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("""abc"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Trim(True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dollar(Empty, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.SumIf("A1:A3", 5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(Array(1, "a", True), 1E+300): s = s & Ap(v)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(Range("Sheet1!A1"), "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SEQUENCE(2,2)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin(Array(1, 2, 3), CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TRUE+1"): s = s & Ap(v)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.VLookup(5, Range("Sheet1!A1:A6"), Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("B1&""x"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("""a""&1.5"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("ROUND(2.5,0)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Sqrt(Range("Sheet1!C1")): s = s & Ap(v)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("SEQUENCE(2,2)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("=1/0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("A1:A2 A2:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("A1>A2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Mod("abc", 2.5): s = s & Ap(v)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Asc(CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsNumber(Array(1, "a", True)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.RoundDown(Range("Sheet1!C1"), CDec("1.5")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Weekday(1E+300, CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.StDev(Range("Sheet1!A1:A6"), Range("Sheet1!C1")): s = s & Ap(v)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("""a""&1.5"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor(Range("Sheet1!B1:B6"), -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Round(2.5, 1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("ISERROR(C1)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even(Range("Sheet1!B4")): s = s & Ap(v) & ErrS()
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("INDEX(A1:A6,3)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Sum(2.5, 5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("SUM(A1:A6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("0.1+0.2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("1,2"): s = s & Ap(v)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Upper(Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor_Math(5, Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(5, CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Sum(2.5, CDec("1.5")): s = s & Ap(v)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Substitute(0, CLng(3), Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.VLookup(5, #1/2/2000#, 1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Even(CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("A1>A2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fixed(Empty, CCur(2.25)): s = s & Ap(v) & ErrS()
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("MAX(B1:B6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dollar("abc", CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("A1:A3*2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Clean("abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(-1): s = s & Ap(v) & ErrS()
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Product(Range("Sheet1!B4"), CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Ln(#1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Index(CCur(2.25), "abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dollar(CLng(3), CLng(3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Odd(Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fixed(CLng(3), CInt(2)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Asc(Range("Sheet1!A1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult(0, Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Trim(Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, 0, CLng(3)): s = s & Ap(v)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(CVErr(2042), #1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("A1>A2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math("3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Transpose(CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("INDEX(A1:A6,3)"): s = s & Ap(v) & ErrS()
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.CountA(CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin("abc", Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(CVErr(2042), "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult(#1/2/2000#, "abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.CountIf(5, -1): s = s & Ap(v)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dollar(-1, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.N(2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.VLookup(Range("Sheet1!B4"), CSng(0.1), CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sign(Range("Sheet1!A1")): s = s & Ap(v) & ErrS()
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EDate(CLng(3), Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(#1/2/2000#, Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Days360(#1/2/2000#, CDec("1.5")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Power(CVErr(2042), 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Rank_Eq(CDec("1.5"), True): s = s & Ap(v) & ErrS()
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Rank_Eq(Array(1, 2, 3), CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Bin2Dec(CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult(1E+300, Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Days360("abc", CSng(0.1)): s = s & Ap(v) & ErrS()
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.SumIf(CCur(2.25), CLng(3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("NA()"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Average("A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.SumIf(CVErr(2042), Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("-2^2"): s = s & Ap(v)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Clean(Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Rept(CDec("1.5"), #1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("=1/0"): s = s & Ap(v)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Median("3", -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Count(Range("Sheet1!B1:B6"), Range("Sheet1!C1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Gcd(CSng(0.1), 1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Asc(#1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("1E308*10"): s = s & Ap(v)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Sum("abc", Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Bin2Dec("A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Percentile(Range("Sheet1!C1"), Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.VLookup(1E+300, 1E+300, CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.StDev(CCur(2.25)): s = s & Ap(v) & ErrS()
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.EoMonth("3", CInt(2)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max("A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.EDate(Empty, 1E+300): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Concat(CCur(2.25), "A1:A3"): s = s & Ap(v)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.RoundDown(5, CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.StDev(5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor_Math(Array(1, 2, 3)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("""a""&1.5"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Dollar(-1, Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product("A1:A3", True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Quotient(CSng(0.1), CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("""a""&1.5"): s = s & Ap(v) & ErrS()
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Days360(-1, Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Match(Range("Sheet1!B4"), "abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Small(CSng(0.1), "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.StDev(0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Ln(1E+300): s = s & Ap(v)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.EDate(Range("Sheet1!B4"), 5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("TEXT(1234.5,""#,##0.00"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Gcd(#1/2/2000#, "A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text(CVErr(2042), CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("CHAR(65)"): s = s & Ap(v) & ErrS()
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Upper(2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Median(0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("A1:A3*2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Power(2.5, 1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor_Math(Range("Sheet1!A1")): s = s & Ap(v)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Odd("abc"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Dollar(CCur(2.25), Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Dec2Bin(Array(1, "a", True), CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.N("A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, Range("Sheet1!B1:B6"), 0): s = s & Ap(v)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("x"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Fact(#1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm(CCur(2.25), 0): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm(#1/2/2000#, 1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EoMonth(Empty, Range("Sheet1!A1:A6")): s = s & Ap(v)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("ROUND(2.5,0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Upper(0): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.RoundDown(Range("Sheet1!A1:A6"), CCur(2.25)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Sqrt(Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("MAX(B1:B6)"): s = s & Ap(v) & ErrS()
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling(CInt(2), CCur(2.25)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Match(True, Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("INDEX(A1:A6,3)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("A1>A2"): s = s & Ap(v) & ErrS()
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Clean(CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Trim(5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("2^0.5"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dec2Bin("A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Max(CVErr(2042)): s = s & Ap(v)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountA(Range("Sheet1!A1:A6"), Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.RoundDown(Range("Sheet1!A1"), -1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("""abc"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Median(1E+300, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Combin(Empty, Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("SEQUENCE(2,2)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Power(True, Empty): s = s & Ap(v) & ErrS()
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm(-1, Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Abs(CCur(2.25)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Bin2Dec("abc"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.StDev(0, Empty): s = s & Ap(v)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("TRUE+1"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Proper(Empty): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Proper(Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Average(CInt(2), CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(CDec("1.5")): s = s & Ap(v)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("x"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor(Range("Sheet1!A1:A6"), Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("=-""3"""): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("A1:A3*2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Sheet1.Evaluate("#REF!"): s = s & Ap(v) & ErrS()
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.CountIf(Range("Sheet1!A1:A6"), CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Sign(Array(1, "a", True)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product(CInt(2)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even(Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Median(CLng(3), 0): s = s & Ap(v) & ErrS()
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsErr(Array(1, "a", True)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.RoundDown(2.5, Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Quotient(Range("Sheet1!B1:B6"), Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EDate(2.5, Array(1, "a", True)): s = s & Ap(v) & ErrS()
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Round("A1:A3", "A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor_Math(Range("Sheet1!B1:B6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Large(Range("Sheet1!B4"), Array(1, "a", True)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("B1&""x"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("0.1+0.2"): s = s & Ap(v) & ErrS()
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.StDev(Range("Sheet1!B1:B6"), Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Percentile(Range("Sheet1!B1:B6"), CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("Sheet1!A2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.N("3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Fact(Range("Sheet1!A1")): s = s & Ap(v) & ErrS()
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Sign(1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("1+1"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EDate(True, Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("NA()"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("Sheet1!A2"): s = s & Ap(v)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.CountIf(0, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Text("3", CDec("1.5")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("1+1"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Large(#1/2/2000#, Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sum(2.5): s = s & Ap(v) & ErrS()
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Sqrt(Range("Sheet1!C1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Trim(2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Match(CCur(2.25), 1E+300): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("-2^2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Upper("3"): s = s & Ap(v)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Mod(Array(1, 2, 3), -1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Clean(Range("Sheet1!C1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.VLookup(Range("Sheet1!B4"), True, Range("Sheet1!A1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Small(2.5, "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Dec2Bin(CSng(0.1), -1): s = s & Ap(v)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Abs(Range("Sheet1!A1:A6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Sqrt(0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("=1/0"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor(0, CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsErr(2.5): s = s & Ap(v) & ErrS()
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("DATE(2000,1,1)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Odd(True): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Count(CInt(2), #1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Rept(Empty, -1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.StDev(CSng(0.1), 2.5): s = s & Ap(v)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ceiling_Math(CVErr(2042)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Weekday(0): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.SumIf(CSng(0.1), 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even(True): s = s & Ap(v)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Upper(CInt(2)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("x"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even(Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("Sheet1!A2"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.N(5): s = s & Ap(v) & ErrS()
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Median("A1:A3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Even(Range("Sheet1!B1:B6")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("SUMPRODUCT(A1:A3,A4:A6)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Substitute(CLng(3), 5, CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Match(CLng(3), CVErr(2042)): s = s & Ap(v) & ErrS()
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Fact(Array(1, 2, 3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Mod(5, CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Text(CLng(3), Range("Sheet1!A1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Weekday("abc", CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v) & ErrS()
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("{1,2;3,4}"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Proper(Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("DATE(2000,1,1)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Weekday(CLng(3), "abc"): s = s & Ap(v) & ErrS()
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Substitute(Empty, -1, Empty): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Text(CCur(2.25), CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Mod(1E+300, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("IFERROR(1/0,""e"")"): s = s & Ap(v)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Asc(CLng(3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Dec2Bin(CDec("1.5"), Array(1, 2, 3)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Index(CCur(2.25), #1/2/2000#): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Floor(CLng(3), CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Average(Array(1, "a", True), "A1:A3"): s = s & Ap(v) & ErrS()
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Sheet1.Evaluate("MAX(B1:B6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.RoundDown(Range("Sheet1!A1:A6"), Range("Sheet1!A1:A6")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("""a""&1.5"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Text(CDec("1.5"), CVErr(2042)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even(Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.MMult(-1, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Mod(CCur(2.25), True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsNumber(CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("""a""&1.5"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Power("A1:A3", 5): s = s & Ap(v)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("B1&""x"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Gcd(CInt(2), "3"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.VLookup(CCur(2.25), Empty, CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.TextJoin(",", True, CDec("1.5"), -1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("#REF!"): s = s & Ap(v)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.SumIf(CCur(2.25), 5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Fact(CCur(2.25)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Ln(CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsText(#1/2/2000#): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Median("3", CCur(2.25)): s = s & Ap(v) & ErrS()
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dollar(CLng(3), 2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Product(CVErr(2042), "3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Proper(CDec("1.5")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("NA()"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("DATE(2000,1,1)"): s = s & Ap(v)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Even("3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("TRUE+1"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Lcm(CCur(2.25), "A1:A3"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("#REF!"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("x"): s = s & Ap(v)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("SUM(A1:A6)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Sqrt(CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Rept(CVErr(2042), True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("ISERROR(C1)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Dec2Bin(Range("Sheet1!A1")): s = s & Ap(v)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.IsErr(Range("Sheet1!B4")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Sheet1.Evaluate("B1&""x"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Ln(CCur(2.25)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Transpose(0): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Rept(2.5, 2.5): s = s & Ap(v) & ErrS()
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.Evaluate("ISERROR(C1)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("Sheet1!A2"): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Clean(-1): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("=-""3"""): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Sign(2.5): s = s & Ap(v) & ErrS()
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = WorksheetFunction.Median(Range("Sheet1!A1:A6"), True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Gcd(Range("Sheet1!C1"), 2.5): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Dollar(Range("Sheet1!C1"), CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Clean(5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Substitute(Range("Sheet1!C1"), Array(1, "a", True), #1/2/2000#): s = s & Ap(v)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.Trim(CSng(0.1)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.WorksheetFunction.N(Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Lcm(CInt(2), Range("Sheet1!C1")): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Text(Array(1, "a", True), CSng(0.1)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.Evaluate("TRUE+1"): s = s & Ap(v)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Application.WorksheetFunction.IsText(Range("Sheet1!B4")): s = s & Ap(v)
    Err.Clear: v = Empty: v = WorksheetFunction.Product(Array(1, "a", True), CInt(2)): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = Application.WorksheetFunction.EoMonth("A1:A3", CCur(2.25)): s = s & Ap(v)
    Err.Clear: v = Empty: v = Application.Evaluate("ROUND(2.5,0)"): s = s & Ap(v) & ErrS()
    Err.Clear: v = Empty: v = WorksheetFunction.Power(CCur(2.25), Range("Sheet1!A1")): s = s & Ap(v)
    C149 = s
End Function
