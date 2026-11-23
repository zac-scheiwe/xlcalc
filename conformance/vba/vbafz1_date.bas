Attribute VB_Name = "ModVbFz1Date"
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
Public Function VbFz1Date(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1Date = C0()
    Case 1: VbFz1Date = C1()
    Case 2: VbFz1Date = C2()
    Case 3: VbFz1Date = C3()
    Case 4: VbFz1Date = C4()
    Case 5: VbFz1Date = C5()
    Case 6: VbFz1Date = C6()
    Case 7: VbFz1Date = C7()
    Case 8: VbFz1Date = C8()
    Case 9: VbFz1Date = C9()
    Case 10: VbFz1Date = C10()
    Case 11: VbFz1Date = C11()
    Case 12: VbFz1Date = C12()
    Case 13: VbFz1Date = C13()
    Case 14: VbFz1Date = C14()
    Case 15: VbFz1Date = C15()
    Case 16: VbFz1Date = C16()
    Case 17: VbFz1Date = C17()
    Case 18: VbFz1Date = C18()
    Case 19: VbFz1Date = C19()
    Case 20: VbFz1Date = C20()
    Case 21: VbFz1Date = C21()
    Case 22: VbFz1Date = C22()
    Case 23: VbFz1Date = C23()
    Case 24: VbFz1Date = C24()
    Case 25: VbFz1Date = C25()
    Case 26: VbFz1Date = C26()
    Case 27: VbFz1Date = C27()
    Case 28: VbFz1Date = C28()
    Case 29: VbFz1Date = C29()
    Case 30: VbFz1Date = C30()
    Case 31: VbFz1Date = C31()
    Case 32: VbFz1Date = C32()
    Case 33: VbFz1Date = C33()
    Case 34: VbFz1Date = C34()
    Case 35: VbFz1Date = C35()
    Case 36: VbFz1Date = C36()
    Case 37: VbFz1Date = C37()
    Case 38: VbFz1Date = C38()
    Case 39: VbFz1Date = C39()
    Case 40: VbFz1Date = C40()
    Case 41: VbFz1Date = C41()
    Case 42: VbFz1Date = C42()
    Case 43: VbFz1Date = C43()
    Case 44: VbFz1Date = C44()
    Case 45: VbFz1Date = C45()
    Case 46: VbFz1Date = C46()
    Case 47: VbFz1Date = C47()
    Case 48: VbFz1Date = C48()
    Case 49: VbFz1Date = C49()
    Case 50: VbFz1Date = C50()
    Case 51: VbFz1Date = C51()
    Case 52: VbFz1Date = C52()
    Case 53: VbFz1Date = C53()
    Case 54: VbFz1Date = C54()
    Case 55: VbFz1Date = C55()
    Case 56: VbFz1Date = C56()
    Case 57: VbFz1Date = C57()
    Case 58: VbFz1Date = C58()
    Case 59: VbFz1Date = C59()
    Case 60: VbFz1Date = C60()
    Case 61: VbFz1Date = C61()
    Case 62: VbFz1Date = C62()
    Case 63: VbFz1Date = C63()
    Case 64: VbFz1Date = C64()
    Case 65: VbFz1Date = C65()
    Case 66: VbFz1Date = C66()
    Case 67: VbFz1Date = C67()
    Case 68: VbFz1Date = C68()
    Case 69: VbFz1Date = C69()
    Case 70: VbFz1Date = C70()
    Case 71: VbFz1Date = C71()
    Case 72: VbFz1Date = C72()
    Case 73: VbFz1Date = C73()
    Case 74: VbFz1Date = C74()
    Case 75: VbFz1Date = C75()
    Case 76: VbFz1Date = C76()
    Case 77: VbFz1Date = C77()
    Case 78: VbFz1Date = C78()
    Case 79: VbFz1Date = C79()
    Case 80: VbFz1Date = C80()
    Case 81: VbFz1Date = C81()
    Case 82: VbFz1Date = C82()
    Case 83: VbFz1Date = C83()
    Case 84: VbFz1Date = C84()
    Case 85: VbFz1Date = C85()
    Case 86: VbFz1Date = C86()
    Case 87: VbFz1Date = C87()
    Case 88: VbFz1Date = C88()
    Case 89: VbFz1Date = C89()
    Case 90: VbFz1Date = C90()
    Case 91: VbFz1Date = C91()
    Case 92: VbFz1Date = C92()
    Case 93: VbFz1Date = C93()
    Case 94: VbFz1Date = C94()
    Case 95: VbFz1Date = C95()
    Case 96: VbFz1Date = C96()
    Case 97: VbFz1Date = C97()
    Case 98: VbFz1Date = C98()
    Case 99: VbFz1Date = C99()
    Case 100: VbFz1Date = C100()
    Case 101: VbFz1Date = C101()
    Case 102: VbFz1Date = C102()
    Case 103: VbFz1Date = C103()
    Case 104: VbFz1Date = C104()
    Case 105: VbFz1Date = C105()
    Case 106: VbFz1Date = C106()
    Case 107: VbFz1Date = C107()
    Case 108: VbFz1Date = C108()
    Case 109: VbFz1Date = C109()
    Case 110: VbFz1Date = C110()
    Case 111: VbFz1Date = C111()
    Case 112: VbFz1Date = C112()
    Case 113: VbFz1Date = C113()
    Case 114: VbFz1Date = C114()
    Case 115: VbFz1Date = C115()
    Case 116: VbFz1Date = C116()
    Case 117: VbFz1Date = C117()
    Case 118: VbFz1Date = C118()
    Case 119: VbFz1Date = C119()
    Case 120: VbFz1Date = C120()
    Case 121: VbFz1Date = C121()
    Case 122: VbFz1Date = C122()
    Case 123: VbFz1Date = C123()
    Case 124: VbFz1Date = C124()
    Case 125: VbFz1Date = C125()
    Case 126: VbFz1Date = C126()
    Case 127: VbFz1Date = C127()
    Case 128: VbFz1Date = C128()
    Case 129: VbFz1Date = C129()
    Case 130: VbFz1Date = C130()
    Case 131: VbFz1Date = C131()
    Case 132: VbFz1Date = C132()
    Case 133: VbFz1Date = C133()
    Case 134: VbFz1Date = C134()
    Case 135: VbFz1Date = C135()
    Case 136: VbFz1Date = C136()
    Case 137: VbFz1Date = C137()
    Case 138: VbFz1Date = C138()
    Case 139: VbFz1Date = C139()
    Case 140: VbFz1Date = C140()
    Case 141: VbFz1Date = C141()
    Case 142: VbFz1Date = C142()
    Case 143: VbFz1Date = C143()
    Case 144: VbFz1Date = C144()
    Case 145: VbFz1Date = C145()
    Case 146: VbFz1Date = C146()
    Case 147: VbFz1Date = C147()
    Case 148: VbFz1Date = C148()
    Case 149: VbFz1Date = C149()
    Case 150: VbFz1Date = C150()
    Case 151: VbFz1Date = C151()
    Case 152: VbFz1Date = C152()
    Case 153: VbFz1Date = C153()
    Case 154: VbFz1Date = C154()
    Case 155: VbFz1Date = C155()
    Case 156: VbFz1Date = C156()
    Case 157: VbFz1Date = C157()
    Case 158: VbFz1Date = C158()
    Case 159: VbFz1Date = C159()
    End Select
    Exit Function
EH:
    VbFz1Date = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 19255.37: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Minute(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(3, 16, 1772): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("w", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(3): s = s & Ap(v)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(57481): d2 = #12/31/9999 11:59:59 PM#
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-7, -31, 2492): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #6:00:00 AM#: d2 = #9/17/2019#
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2, 8): s = s & Ap(v)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1): s = s & Ap(v)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = Null
    Err.Clear: v = Empty: v = DateAdd("ww", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, 4, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(7, -40, -1515): s = s & Ap(v)
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(78871.759): d2 = Null
    Err.Clear: v = Empty: v = DatePart("q", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-30, -56, 1230): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/8/2023#: d2 = "1/2/2000"
    Err.Clear: v = Empty: v = TimeSerial(-12, 85, -1291): s = s & Ap(v)
    Err.Clear: v = Empty: v = CDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 1, vbFirstFourDays): s = s & Ap(v)
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(55): d2 = #9/2/1994#
    Err.Clear: v = Empty: v = DatePart("w", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(0, 0, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(6, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-26, -87, -469): s = s & Ap(v)
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(5050.11): d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, -12, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(8, False): s = s & Ap(v)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #2/29/2000 12:00:00 PM#: d2 = 17786.42
    Err.Clear: v = Empty: v = DatePart("q", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(10000, 12, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/1/2000#: d2 = #1/1/100#
    Err.Clear: v = Empty: v = WeekdayName(3): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(1, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(28, -12, 1708): s = s & Ap(v)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 12281.61: d2 = "x"
    Err.Clear: v = Empty: v = DateAdd("D", 10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(6, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(5, False, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(0): s = s & Ap(v)
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = #1/1/2000#
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(29.5, -1, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateValue(d1): s = s & Ap(v)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #6:00:00 AM#: d2 = CDate(55424.85011)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = CDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(1, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(7, False, 2): s = s & Ap(v)
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #8/19/1992#: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(-1, 13, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(9, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(0, 34, 0): s = s & Ap(v)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = "1/2/2000"
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(6, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, 0, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-4, 3, -681): s = s & Ap(v)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(18, 18, 3582): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-24, 14, 1342): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("m", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbMonday): s = s & Ap(v)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 1334.19: d2 = #6/14/2018#
    Err.Clear: v = Empty: v = DateSerial(2028, 0, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-7, 62, -1879): s = s & Ap(v)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = 40741.6
    Err.Clear: v = Empty: v = DateSerial(100, 13, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(2, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, vbSunday): s = s & Ap(v)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = #10/13/2021#
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 0, 4): s = s & Ap(v)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = #6:00:00 AM#
    Err.Clear: v = Empty: v = DatePart("ww", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(6): s = s & Ap(v)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, vbSunday): s = s & Ap(v)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #6/10/2011#: d2 = "1/2/2000"
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(9, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(10, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(8, 26, -3082): s = s & Ap(v)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/1/2000#: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateAdd("D", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("YYYY", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1, 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 2.5, d1): s = s & Ap(v)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 26460.39: d2 = #12/31/9999 11:59:59 PM#
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = Month(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = #12/31/1899#
    Err.Clear: v = Empty: v = TimeSerial(-17, 50, 2118): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("yyyy", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Minute(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 4, 2): s = s & Ap(v)
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/1/2000#: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, 2.5, 32): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(59446.9): d2 = Null
    Err.Clear: v = Empty: v = DateAdd("q", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(3, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2): s = s & Ap(v)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = Null
    Err.Clear: v = Empty: v = DatePart("q", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("m", d1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("w", -4602, d1): s = s & Ap(v)
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 36195.46: d2 = Null
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(-1, 2.5, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = Minute(d1): s = s & Ap(v)
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 27962.04: d2 = Null
    Err.Clear: v = Empty: v = MonthName(8, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(7, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(0, 2.5, 0.5): s = s & Ap(v)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #5/6/2014#: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DatePart("YYYY", d1, 7, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(3, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(32, 74, 3140): s = s & Ap(v)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #10/9/1994#: d2 = #2/29/2000 12:00:00 PM#
    Err.Clear: v = Empty: v = DateSerial(-1, 13, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("s", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 6, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, 0, 4): s = s & Ap(v)
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = CDate(26113.2)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("m", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(36, -60, -1315): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 1, vbFirstFourDays): s = s & Ap(v)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = "x"
    Err.Clear: v = Empty: v = TimeSerial(-10, 80, -3676): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(20, 54, -1266): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("h", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4, False, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 19089.66: d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(1, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(86, 1.5, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Month(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-12, -58, -514): s = s & Ap(v)
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/1/100#: d2 = Null
    Err.Clear: v = Empty: v = DatePart("", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = Minute(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(31, -98, 3383): s = s & Ap(v)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #4/21/2000#: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DatePart("h", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(99, 2.5, 32): s = s & Ap(v)
    Err.Clear: v = Empty: v = CDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("x", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 8): s = s & Ap(v)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = #6:00:00 AM#
    Err.Clear: v = Empty: v = MonthName(11, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(100, -1, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(9): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-1, 93, -3393): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("YYYY", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("m", 1696, d1): s = s & Ap(v)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/31/1899#: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("", 2.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(3, False, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 46492.55: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(6): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #5/3/2003#: d2 = 38619.03
    Err.Clear: v = Empty: v = Second(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("ww", 1E+10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = "1/2/2000"
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("s", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("q", 1E+10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("D", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(6): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = CDate(48725.453)
    Err.Clear: v = Empty: v = DatePart("", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, 12, 218): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(9): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, vbMonday): s = s & Ap(v)
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 203.61: d2 = #12/31/9999 11:59:59 PM#
    Err.Clear: v = Empty: v = DateSerial(9999, 2.5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(13, 1.5, -156): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(43, 8, 717): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("ww", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 8): s = s & Ap(v)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = CDate(39708.25)
    Err.Clear: v = Empty: v = DateSerial(29.5, 2.5, -287): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(44, 79, 287): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 4, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 14708.48: d2 = #12/19/1993#
    Err.Clear: v = Empty: v = DateSerial(-1, 12, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(2020, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, 2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("h", d1, 7, 1): s = s & Ap(v)
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(78, 34, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, 29, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-1, 18, -2511): s = s & Ap(v)
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = "x"
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("n", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("h", d1, 0, 4): s = s & Ap(v)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = "x"
    Err.Clear: v = Empty: v = Hour(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("y", d1, 1, vbFirstFourDays): s = s & Ap(v)
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(35346.56652): d2 = #12/30/1899#
    Err.Clear: v = Empty: v = DateDiff("", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(2083, 1.5, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(26, 77, 1204): s = s & Ap(v)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = 21643.87
    Err.Clear: v = Empty: v = DatePart("d", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1, 1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-6, 10, -686): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("h", d1): s = s & Ap(v)
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/1/100#: d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("y", d1, 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(5, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = "x"
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1): s = s & Ap(v)
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 3328.87: d2 = DateSerial(2059, 4, 11)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, -1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("y", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(16, -16, 431): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = #12/30/1899#
    Err.Clear: v = Empty: v = DateSerial(-1, 1.5, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(15, 66, 3163): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 1, vbFirstFourDays): s = s & Ap(v)
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = CDate(4021.0365)
    Err.Clear: v = Empty: v = DateDiff("D", d1, d2, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("YYYY", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-1, 20, 2880): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 7, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(35, 57, -663): s = s & Ap(v)
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(16324): d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DatePart("h", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-23, -61, 1245): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 1E+10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("w", 2578, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(100, 1.5, 60): s = s & Ap(v)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = CDate(48519.8)
    Err.Clear: v = Empty: v = DateSerial(10000, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-3, -99, -257): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(20, -18, 14): s = s & Ap(v)
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = CDate(75)
    Err.Clear: v = Empty: v = DatePart("M", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1931, 1.5, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(8, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(11): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("D", d1, d2, 2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", 2.5, d1): s = s & Ap(v)
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(13306.907): d2 = DateSerial(2070, 1, 2)
    Err.Clear: v = Empty: v = TimeSerial(12, -59, 1007): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, -1, 32): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", 2, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = Null
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, 1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Second(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2, 6, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("x", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 0): s = s & Ap(v)
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(33587.85): d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateSerial(9999, 2.5, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(29.5, 1.5, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, 13, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("h", d1, 1, vbFirstFourDays): s = s & Ap(v)
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = #12/27/2022#
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("h", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(16, 62, -2310): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(9, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 0, 4): s = s & Ap(v)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 48866.22: d2 = CDate(29009.90407)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(10000, 0, 69): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(3, False, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1, 8): s = s & Ap(v)
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(29899): d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DateSerial(9999, 2.5, 375): s = s & Ap(v)
    Err.Clear: v = Empty: v = Second(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("m", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, vbMonday): s = s & Ap(v)
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DatePart("q", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("M", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(2, -13, 1647): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-19, -17, -2444): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("m", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = CDate(26556.75)
    Err.Clear: v = Empty: v = TimeSerial(20, 8, 2705): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(0, 2.5, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, 6, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("s", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("y", d1): s = s & Ap(v)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/1/100#: d2 = "x"
    Err.Clear: v = Empty: v = DatePart("M", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 6, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 0, 0): s = s & Ap(v)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = #2/3/2002#
    Err.Clear: v = Empty: v = DatePart("", d1, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, 12, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = CDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = CDate(18126.6)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-29, -34, -3091): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = CDate(61865.5)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(0, False): s = s & Ap(v)
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = Null
    Err.Clear: v = Empty: v = DatePart("y", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("D", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(29.5, 16, 32): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(40, 2.5, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = CDate(69)
    Err.Clear: v = Empty: v = DatePart("D", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(6): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("D", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", 1, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("YYYY", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2, vbMonday): s = s & Ap(v)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(47955.6364): d2 = Null
    Err.Clear: v = Empty: v = DatePart("w", d1, 1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-26, -57, -1567): s = s & Ap(v)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(1022.093): d2 = #7/25/2022#
    Err.Clear: v = Empty: v = DateSerial(1.5, -27, 32): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-1, -48, 1451): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("w", -0.5, d1): s = s & Ap(v)
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(12, -61, -3339): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("YYYY", 2.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("s", 1E+10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(7, True): s = s & Ap(v)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/31/9999 11:59:59 PM#: d2 = #6:00:00 AM#
    Err.Clear: v = Empty: v = WeekdayName(5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Minute(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("x", 1581, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("ww", 32768, d1): s = s & Ap(v)
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = Null
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, 4, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(99, 2.5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(10000, 30, 32): s = s & Ap(v)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/31/1899#: d2 = "x"
    Err.Clear: v = Empty: v = DateAdd("d", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", -0.5, d1): s = s & Ap(v)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = #12/31/9999 11:59:59 PM#
    Err.Clear: v = Empty: v = DatePart("", d1, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("YYYY", 1723, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(5, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(6): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = DateSerial(1915, 6, 11): d2 = CDate(46572)
    Err.Clear: v = Empty: v = WeekdayName(3, False, 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, -1, 0.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(6, False, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 1, vbFirstFourDays): s = s & Ap(v)
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #8/18/2016#: d2 = 9461.91
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Day(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 0, 4): s = s & Ap(v)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #5/7/2014#: d2 = 39292.39
    Err.Clear: v = Empty: v = DatePart("yyyy", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", -1.9, d1): s = s & Ap(v)
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/30/1899#: d2 = #1/1/2000#
    Err.Clear: v = Empty: v = TimeSerial(-13, 3, 771): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(10000, 13, 1.5): s = s & Ap(v)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(17772.2): d2 = #3/2/2010#
    Err.Clear: v = Empty: v = TimeSerial(-12, 58, 3303): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(48, 44, 3806): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(7, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, 7): s = s & Ap(v)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #9/4/1998#: d2 = "x"
    Err.Clear: v = Empty: v = MonthName(0, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("q", 2.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 24108.73: d2 = 28077.37
    Err.Clear: v = Empty: v = DateDiff("D", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(10000, 0, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("q", 1E+10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("x", 0, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("YYYY", -2303, d1): s = s & Ap(v)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = #8/27/2008#
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 2, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(100, -1, -1): s = s & Ap(v)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = Null
    Err.Clear: v = Empty: v = DateSerial(0, 6, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 0, d1): s = s & Ap(v)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = CDate(29626.5)
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("ww", 1150, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(2, -10, -681): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(2099, -2, 0.5): s = s & Ap(v)
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #2/16/2005#: d2 = "x"
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = CDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("n", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 0, 4): s = s & Ap(v)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(63700): d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DateSerial(1.5, -1, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(3, 87, 2954): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(11, -17, -3185): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(2, False): s = s & Ap(v)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = Null
    Err.Clear: v = Empty: v = TimeSerial(19, 38, -985): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("ww", 0, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 1920, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1, vbMonday): s = s & Ap(v)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 41440.33: d2 = "x"
    Err.Clear: v = Empty: v = TimeSerial(0, -20, -2696): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 4, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Hour(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("D", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, vbMonday): s = s & Ap(v)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/31/9999 11:59:59 PM#: d2 = CDate(53628.849878)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Month(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Hour(d1): s = s & Ap(v)
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(10465.3): d2 = Null
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", -3807, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(33, 59, -1012): s = s & Ap(v)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = "x"
    Err.Clear: v = Empty: v = TimeSerial(42, 38, 2768): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, -1, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(0, 2.5, 31): s = s & Ap(v)
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = CDate(49)
    Err.Clear: v = Empty: v = TimeSerial(-10, 49, -3422): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(9, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", -0.5, d1): s = s & Ap(v)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(23978.757553): d2 = #1/1/100#
    Err.Clear: v = Empty: v = TimeSerial(37, 70, -1292): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, 1.5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(14, -11, -3302): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("n", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateSerial(99, -1, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Month(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(99, 12, 279): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(3, True): s = s & Ap(v)
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = #1/1/100#
    Err.Clear: v = Empty: v = DateSerial(-1, -14, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(29, 52, -3988): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("s", d1, 5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(-1, 12, -1): s = s & Ap(v)
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(14675.866251): d2 = CDate(2877.4)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("yyyy", 1E+10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("x", d1, 5, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 0): s = s & Ap(v)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = CDate(48284.53827)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("D", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(24, 22, -2367): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("D", d1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("s", -4243, d1): s = s & Ap(v)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/25/2026#: d2 = "x"
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(28, 38, -1763): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/30/1899#: d2 = CDate(47703.2354)
    Err.Clear: v = Empty: v = DateDiff("D", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("w", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(2, 16, 2389): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-9, 71, 504): s = s & Ap(v)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(14405.0808): d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateAdd("x", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(36, -15, -2653): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-17, 28, 1920): s = s & Ap(v)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #10/1/2023#: d2 = Null
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-23, 84, -1485): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(112, 2.5, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2): s = s & Ap(v)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = CDate(38729)
    Err.Clear: v = Empty: v = TimeSerial(-7, -26, 511): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 6, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1982, 1.5, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("D", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-24, 13, 533): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 26046.92: d2 = CDate(8)
    Err.Clear: v = Empty: v = DateAdd("x", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(18, 0, 2740): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, 13, 32): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbMonday): s = s & Ap(v)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = #1/1/2000#
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("q", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(2, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbMonday): s = s & Ap(v)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 31925.68: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("M", 2.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-8, -55, 3135): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(2, False, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(42, 36, 2614): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(915.25): d2 = "x"
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(39, -42, 2835): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(44, -89, -3567): s = s & Ap(v)
    Err.Clear: v = Empty: v = Minute(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = CDate(11584.093083)
    Err.Clear: v = Empty: v = WeekdayName(7, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("M", d1, 5, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 8): s = s & Ap(v)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #2/29/2000 12:00:00 PM#: d2 = Null
    Err.Clear: v = Empty: v = DatePart("q", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(35, -3, 733): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(27, 67, -1124): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = CDate(42605.8512)
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(14, -10, -160): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Day(d1): s = s & Ap(v)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(2): s = s & Ap(v)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(21990.1041): d2 = Null
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-13, 70, -2971): s = s & Ap(v)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/30/1899#: d2 = 29936.69
    Err.Clear: v = Empty: v = DateValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("h", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = CDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("s", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("D", d1, vbMonday): s = s & Ap(v)
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/31/1899#: d2 = "1/2/2000"
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(0, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(40, -53, 558): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #6:00:00 AM#: d2 = 18724.82
    Err.Clear: v = Empty: v = TimeSerial(-6, 93, -3488): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(10000, 12, 194): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("d", 1E+10, d1): s = s & Ap(v)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(29.5, 2.5, 0.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("YYYY", d1, 0, 4): s = s & Ap(v)
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = #1/1/100#
    Err.Clear: v = Empty: v = TimeValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(10, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(36, -24, 2164): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = 33820.54
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 4, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("m", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, 8): s = s & Ap(v)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(26977): d2 = CDate(15629.152616)
    Err.Clear: v = Empty: v = DateAdd("ww", 1.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1944, 0, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("YYYY", d1, 3, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(-1, 13, 0.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("s", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("q", -0.5, d1): s = s & Ap(v)
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/7/2030#: d2 = "x"
    Err.Clear: v = Empty: v = DatePart("q", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, 0, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(29.5, 13, -226): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(1, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 4): s = s & Ap(v)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = DateSerial(2093, 7, 13): d2 = Null
    Err.Clear: v = Empty: v = MonthName(0, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("q", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(99, 15, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-5, 99, 1761): s = s & Ap(v)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #4/5/2030#: d2 = CDate(72)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("YYYY", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2, 7, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("M", d1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, vbMonday): s = s & Ap(v)
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = #12/31/1899#
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(9, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, 13, 0.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Day(d1): s = s & Ap(v)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 5450.87: d2 = #12/31/9999 11:59:59 PM#
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("n", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("YYYY", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(2, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1): s = s & Ap(v)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 20178.62: d2 = CDate(40990.315835)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, -8, 32): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 7, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", 1.9, d1): s = s & Ap(v)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateAdd("s", -4580, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(100, 1.5, 31): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("YYYY", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", 2.5, d1): s = s & Ap(v)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = #1/1/100#
    Err.Clear: v = Empty: v = DatePart("", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("", -1, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = #6/17/2027#
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("n", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = IsDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(-1, 1.5, 60): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("yyyy", d1): s = s & Ap(v)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #6/8/2004#: d2 = #3/24/2002#
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(27, -44, -2418): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(99, 25, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("s", -122, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("YYYY", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1942, 1.5, -1): s = s & Ap(v)
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = #9/9/2001#
    Err.Clear: v = Empty: v = DateAdd("y", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-3, -66, 3175): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(50, -1, 3296): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("m", 2.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("s", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 8): s = s & Ap(v)
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = 43658.75
    Err.Clear: v = Empty: v = MonthName(7, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-14, -38, 2224): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("q", 2.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Day(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Second(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1, vbSunday): s = s & Ap(v)
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/1/100#: d2 = #7/24/1990#
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("h", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(7, False, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 7, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = CDate(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(0, 12, 32): s = s & Ap(v)
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #2/21/2003#: d2 = 13989.96
    Err.Clear: v = Empty: v = WeekdayName(0, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Day(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(26, -25, 1542): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = #2/12/2024#
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = Year(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(0, -19, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(23, 58, -2194): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = CDate(52709.69057): d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DatePart("q", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("m", -1127, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("n", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(5): s = s & Ap(v)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #2/29/2000 12:00:00 PM#: d2 = 28710.66
    Err.Clear: v = Empty: v = DateAdd("", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(8, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = Null
    Err.Clear: v = Empty: v = DateDiff("", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-19, 53, -1886): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2): s = s & Ap(v)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 41952.74: d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DatePart("n", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(5): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = 12849.58: d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1, 6, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 32768, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(18, -51, 966): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(7): s = s & Ap(v)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = "1/2/2000"
    Err.Clear: v = Empty: v = DateAdd("d", -2465, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(100, -1, 391): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(9999, 1.5, -180): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("w", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(10000, 1.5, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4, False, 5): s = s & Ap(v)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #12/30/1899#: d2 = #2/10/2024#
    Err.Clear: v = Empty: v = Month(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, -1, 31): s = s & Ap(v)
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = #2/29/2000 12:00:00 PM#
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, 7, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(-1, 2.5, 32): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("YYYY", 1E+10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("w", 1E+10, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, vbSunday): s = s & Ap(v)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "x": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DatePart("w", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("D", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("n", d1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(5, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("y", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("y", d1, d2, 0, 4): s = s & Ap(v)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "31/12/2001 13:00": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = Day(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("D", -0.5, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("d", d1, d2, 6, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(29.5, 12, 60): s = s & Ap(v)
    C149 = s
End Function
Private Function C150() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = Null
    Err.Clear: v = Empty: v = TimeSerial(7, -73, -705): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateValue(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("x", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(0, False): s = s & Ap(v)
    C150 = s
End Function
Private Function C151() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("y", d1, 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("D", 1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("d", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(1.5, 13, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("m", d1, d2, vbMonday): s = s & Ap(v)
    C151 = s
End Function
Private Function C152() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = CDate(9)
    Err.Clear: v = Empty: v = TimeSerial(11, 6, 1760): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(-1, 12, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 - d2: s = s & Ap(v)
    C152 = s
End Function
Private Function C153() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = CDate(40367.3671)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 + d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(7, True): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 2, vbFirstFullWeek): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 100, d1): s = s & Ap(v)
    C153 = s
End Function
Private Function C154() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #3/7/1998#: d2 = CDate(15712.68937)
    Err.Clear: v = Empty: v = DatePart("q", d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(0, 39, 1730): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(-18, -56, -721): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("M", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("w", d1, d2): s = s & Ap(v)
    C154 = s
End Function
Private Function C155() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = CDate(52095)
    Err.Clear: v = Empty: v = DatePart("ww", d1, 0, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("q", -1.9, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(44, 2.5, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("s", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    C155 = s
End Function
Private Function C156() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #6/16/2000#: d2 = 31100.68
    Err.Clear: v = Empty: v = DatePart("w", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("w", d1, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("ww", d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Month(d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(20, -57, 275): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 = d2: s = s & Ap(v)
    C156 = s
End Function
Private Function C157() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = #1/22/1991#: d2 = #12/31/1899#
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = WeekdayName(5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(15, -68, -29): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("q", d1, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("y", -0.5, d1): s = s & Ap(v)
    C157 = s
End Function
Private Function C158() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = "1/2/2000": d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateDiff("ww", d1, d2, 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Weekday(d1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("q", d1, d2, vbMonday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("yyyy", d1, d2, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = MonthName(12, False): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateAdd("h", 100, d1): s = s & Ap(v)
    C158 = s
End Function
Private Function C159() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim d1 As Variant, d2 As Variant
    d1 = Null: d2 = "31/12/2001 13:00"
    Err.Clear: v = Empty: v = DateAdd("YYYY", -1, d1): s = s & Ap(v)
    Err.Clear: v = Empty: v = TimeSerial(2, -31, 3698): s = s & Ap(v)
    Err.Clear: v = Empty: v = DatePart("m", d1, 1, vbFirstFourDays): s = s & Ap(v)
    Err.Clear: v = Empty: v = d1 < d2: s = s & Ap(v)
    Err.Clear: v = Empty: v = DateDiff("h", d1, d2, vbSunday): s = s & Ap(v)
    Err.Clear: v = Empty: v = DateSerial(10000, -1, 1.5): s = s & Ap(v)
    C159 = s
End Function
