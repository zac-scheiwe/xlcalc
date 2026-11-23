Attribute VB_Name = "VbaR11"
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

Public Function VbEleven(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbEleven = M0()
    Case 1: VbEleven = M1()
    Case 2: VbEleven = M2()
    Case 3: VbEleven = M3()
    Case 4: VbEleven = M4()
    Case 5: VbEleven = M5()
    Case 6: VbEleven = M6()
    Case 7: VbEleven = M7()
    Case 8: VbEleven = M8()
    Case 9: VbEleven = M9()
    Case 10: VbEleven = M10()
    Case 11: VbEleven = M11()
    Case 12: VbEleven = M12()
    Case 13: VbEleven = M13()
    Case 14: VbEleven = M14()
    Case 15: VbEleven = M15()
    Case 16: VbEleven = M16()
    Case 17: VbEleven = M17()
    Case 18: VbEleven = M18()
    Case 19: VbEleven = M19()
    Case 20: VbEleven = M20()
    Case 21: VbEleven = M21()
    Case 22: VbEleven = M22()
    Case 23: VbEleven = M23()
    Case 24: VbEleven = M24()
    Case 25: VbEleven = M25()
    Case 26: VbEleven = M26()
    Case 27: VbEleven = M27()
    Case 28: VbEleven = M28()
    Case 29: VbEleven = M29()
    Case 30: VbEleven = M30()
    Case 31: VbEleven = M31()
    Case 32: VbEleven = M32()
    Case 33: VbEleven = M33()
    Case 34: VbEleven = M34()
    Case 35: VbEleven = M35()
    Case 36: VbEleven = M36()
    Case 37: VbEleven = M37()
    Case 38: VbEleven = M38()
    Case 39: VbEleven = M39()
    Case 40: VbEleven = M40()
    Case 41: VbEleven = M41()
    Case 42: VbEleven = M42()
    Case 43: VbEleven = M43()
    Case 44: VbEleven = M44()
    Case 45: VbEleven = M45()
    Case 46: VbEleven = M46()
    Case 47: VbEleven = M47()
    Case 48: VbEleven = M48()
    Case 49: VbEleven = M49()
    Case 50: VbEleven = M50()
    Case 51: VbEleven = M51()
    Case 52: VbEleven = M52()
    Case 53: VbEleven = M53()
    Case 54: VbEleven = M54()
    Case 55: VbEleven = M55()
    Case 56: VbEleven = M56()
    Case 57: VbEleven = M57()
    Case 58: VbEleven = M58()
    Case 59: VbEleven = M59()
    Case 60: VbEleven = M60()
    Case 61: VbEleven = M61()
    Case 62: VbEleven = M62()
    Case 63: VbEleven = M63()
    Case 64: VbEleven = M64()
    Case 65: VbEleven = M65()
    Case 66: VbEleven = M66()
    Case 67: VbEleven = M67()
    Case 68: VbEleven = M68()
    Case 69: VbEleven = M69()
    Case 70: VbEleven = M70()
    Case 71: VbEleven = M71()
    Case 72: VbEleven = M72()
    Case 73: VbEleven = M73()
    Case 74: VbEleven = M74()
    Case 75: VbEleven = M75()
    Case 76: VbEleven = M76()
    Case 77: VbEleven = M77()
    Case 78: VbEleven = M78()
    Case 79: VbEleven = M79()
    Case 80: VbEleven = M80()
    Case 81: VbEleven = M81()
    Case 82: VbEleven = M82()
    Case 83: VbEleven = M83()
    Case 84: VbEleven = M84()
    Case 85: VbEleven = M85()
    Case 86: VbEleven = M86()
    Case 87: VbEleven = M87()
    Case 88: VbEleven = M88()
    Case 89: VbEleven = M89()
    Case 90: VbEleven = M90()
    Case 91: VbEleven = M91()
    Case 92: VbEleven = M92()
    Case 93: VbEleven = M93()
    Case 94: VbEleven = M94()
    Case 95: VbEleven = M95()
    Case 96: VbEleven = M96()
    Case 97: VbEleven = M97()
    Case 98: VbEleven = M98()
    Case 99: VbEleven = M99()
    Case 100: VbEleven = M100()
    Case 101: VbEleven = M101()
    Case 102: VbEleven = M102()
    Case 103: VbEleven = M103()
    Case 104: VbEleven = M104()
    Case 105: VbEleven = M105()
    Case 106: VbEleven = M106()
    Case 107: VbEleven = M107()
    End Select
    Exit Function
EH:
    VbEleven = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M0() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, s As String
    For Each v In Array(0, 1234567.891, -0.5, 0.005, 1E+15, 1E-5, 12.5, 0.999)
        s = s & Hx(FormatNumber(v)) & Hx(FormatNumber(v, 0)) & Hx(FormatNumber(v, 3, vbFalse, vbTrue, vbFalse)) & Hx(FormatNumber(v, -1, vbTrue, vbTrue, vbTrue)) & ";"
    Next v
    R = s
Done:
    M0 = R
    Exit Function
EH:
    M0 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M1() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, s As String
    For Each v In Array(0, 1234567.891, -0.5, 0.005, 12.5, -1234.5)
        s = s & Hx(FormatCurrency(v)) & Hx(FormatCurrency(v, 0)) & Hx(FormatCurrency(v, 1, vbTrue, vbTrue, vbFalse)) & Hx(FormatCurrency(v, , vbFalse, vbFalse, vbTrue)) & ";"
    Next v
    R = s
Done:
    M1 = R
    Exit Function
EH:
    M1 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M2() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, s As String
    For Each v In Array(0, 0.256, 1.5, -0.005, 12, 1E-5)
        s = s & Hx(FormatPercent(v)) & Hx(FormatPercent(v, 0)) & Hx(FormatPercent(v, 3, vbFalse, vbTrue, vbFalse)) & Hx(FormatPercent(v, 1, vbTrue, vbTrue, vbTrue)) & ";"
    Next v
    R = s
Done:
    M2 = R
    Exit Function
EH:
    M2 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M3() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, s As String, i As Long
    For Each v In Array(#1/2/2000 3:04:05 PM#, #12/31/1999#, #6:30:00 AM#, 36526.75, 0.5)
        For i = 0 To 4
            s = s & Hx(FormatDateTime(v, i)) & ","
        Next i
        s = s & ";"
    Next v
    R = s
Done:
    M3 = R
    Exit Function
EH:
    M3 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M4() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    R = Hx(FormatNumber("abc")) & Err.Number & ErrS() & Hx(FormatNumber(Null)) & Err.Number & Hx(FormatNumber(Empty)) & Hx(FormatNumber("")) & Err.Number & Hx(FormatNumber(1, 99)) & Err.Number & Hx(FormatNumber(1, 2, 5, 5, 5)) & Err.Number & Hx(FormatDateTime("x", 1)) & Err.Number & Hx(FormatDateTime(1, 9)) & Err.Number
Done:
    M4 = R
    Exit Function
EH:
    M4 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M5() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, s As String
    For Each v In Array("12", " 12 ", "1,234", "1e3", "&H10", "&O17", "12abc", "abc", "", " ", "1 2", ".5", "-.5e-2", "+5", "1d2", "1.5.5", "&HFFFFFFFF", "$5", "(5)", "5%", "- 5", "1e", "--5", "0x10")
        s = s & Dmp(Val(v)) & Hx(Str(Val(v))) & ";"
    Next v
    R = s
Done:
    M5 = R
    Exit Function
EH:
    M5 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M6() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, s As String
    For Each v In Array(0, 1, -1, 1.5, 0.1, 1E+20, 1E-20, 123456789.123456789, True, CCur(5.25), CDec("1.0"), CSng(0.1), 1E+15, 1E+14, 0.0001, 0.00001, -0#)
        s = s & Hx(Str(v)) & Hx(CStr(v)) & Hx(CStr(CDbl(v)) & "") & ";"
    Next v
    R = s
Done:
    M6 = R
    Exit Function
EH:
    M6 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M7() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Dmp(Date - Date)
    s = s & Dmp(CDate(0))
    s = s & Dmp(CDate(1))
    s = s & Dmp(CDate(-1))
    s = s & Dmp(CDate(60))
    s = s & Dmp(CDate(59.999999))
    s = s & Dmp(CDate(2958465))
    s = s & Dmp(CDate(0.99999999))
    On Error Resume Next
    s = s & Dmp(CDate(2958466))
    s = s & ErrS()
    s = s & Dmp(CDate(-657435))
    s = s & ErrS()
    s = s & Dmp(CDate(-657436))
    s = s & ErrS()
    s = s & Dmp(CDate(""))
    s = s & ErrS()
    s = s & Dmp(CDate("13/13/2000"))
    s = s & ErrS()
    R = s
Done:
    M7 = R
    Exit Function
EH:
    M7 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M8() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    On Error Resume Next
    For Each v In Array("1/2/2000", "2000-01-02", "Jan 2, 2000", "2 Jan 2000", "1/2", "Jan 2", "2000", "12:30", "12:30 PM", "1:2:3", "25:00", "1/2/2000 12:30", "January 2000", "Feb 30", "2/29/1900", "2/29/2001", "1/1/100", "1/1/30", "1/1/29", "1/1/00", "T12", "5")
        s = s & Dmp(CDate(v))
        s = s & ErrS()
        s = s & ";"
    Next v
    R = s
Done:
    M8 = R
    Exit Function
EH:
    M8 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M9() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, d As Date
    d = #2/29/2000 11:59:59 PM#
    s = Dmp(DateAdd("m", 12, d))
    s = s & Dmp(DateAdd("yyyy", 1, d))
    s = s & Dmp(DateAdd("q", 1, d))
    s = s & Dmp(DateAdd("ww", 1, d))
    s = s & Dmp(DateAdd("h", 1, d))
    s = s & Dmp(DateAdd("s", -86400, d))
    s = s & Dmp(DateAdd("d", 1.7, d))
    s = s & Dmp(DateAdd("n", 0.5, d))
    s = s & Dmp(DateAdd("w", 1, d))
    s = s & Dmp(DateAdd("y", 1, d))
    On Error Resume Next
    s = s & Dmp(DateAdd("x", 1, d))
    s = s & ErrS()
    s = s & Dmp(DateAdd("yyyy", 8000, d))
    s = s & ErrS()
    s = s & Dmp(DateAdd("m", 1, Null))
    s = s & ErrS()
    R = s
Done:
    M9 = R
    Exit Function
EH:
    M9 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M10() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Date, b As Date, i As Long, u As Variant
    a = #12/31/1999 11:59:59 PM#: b = #1/1/2000 12:00:01 AM#
    For Each u In Array("yyyy", "q", "m", "y", "d", "w", "ww", "h", "n", "s")
        s = s & u & "=" & DateDiff(u, a, b) & "/" & DateDiff(u, b, a) & "/" & DateDiff(u, a, b, vbMonday, vbFirstFourDays) & "/" & DateDiff(u, a, b, vbSunday, vbFirstFullWeek) & ";"
    Next u
    R = s
Done:
    M10 = R
    Exit Function
EH:
    M10 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M11() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Date, u As Variant
    a = #1/1/2000 6:07:08 AM#
    For Each u In Array("yyyy", "q", "m", "y", "d", "w", "ww", "h", "n", "s")
        s = s & u & "=" & DatePart(u, a) & "/" & DatePart(u, a, vbMonday) & "/" & DatePart(u, a, vbMonday, vbFirstFourDays) & "/" & DatePart(u, a, vbSunday, vbFirstFullWeek) & "/" & DatePart(u, a, vbSaturday, vbFirstJan1) & ";"
    Next u
    R = s
Done:
    M11 = R
    Exit Function
EH:
    M11 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M12() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 0 To 8
        s = s & Weekday(#1/1/2000#, i)
        s = s & WeekdayName(i, False, i)
        s = s & ","
    Next i
    On Error Resume Next
    s = s & WeekdayName(0)
    s = s & ErrS()
    s = s & MonthName(13)
    s = s & ErrS()
    s = s & MonthName(0)
    s = s & ErrS()
    s = s & MonthName(12, True)
    s = s & WeekdayName(7, True, vbMonday)
    s = s & WeekdayName(8)
    s = s & ErrS()
    R = s
Done:
    M12 = R
    Exit Function
EH:
    M12 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M13() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Dmp(DateSerial(2000, 14, 35))
    s = s & Dmp(DateSerial(2000, 0, 0))
    s = s & Dmp(DateSerial(99, 1, 1))
    s = s & Dmp(DateSerial(0, 1, 1))
    s = s & Dmp(DateSerial(100, 1, 1))
    s = s & Dmp(DateSerial(1899, 12, 30))
    s = s & Dmp(DateSerial(-1, 1, 1))
    s = s & ErrS()
    s = s & Dmp(DateSerial(9999, 12, 31))
    s = s & Dmp(DateSerial(9999, 12, 32))
    s = s & ErrS()
    s = s & Dmp(DateSerial(30, 1, 1))
    s = s & Dmp(DateSerial(29, 1, 1))
    s = s & Dmp(TimeSerial(25, 61, 61))
    s = s & Dmp(TimeSerial(-1, 0, 0))
    s = s & Dmp(TimeSerial(0, 0, -1))
    s = s & Dmp(DateValue("1/2/2000 3:04"))
    s = s & Dmp(TimeValue("1/2/2000 3:04"))
    s = s & ErrS()
    s = s & Dmp(TimeValue("25:00"))
    s = s & ErrS()
    R = s
Done:
    M13 = R
    Exit Function
EH:
    M13 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M14() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Dmp(Year(0))
    s = s & Dmp(Month(0))
    s = s & Dmp(Day(0))
    s = s & Dmp(Weekday(0))
    s = s & Dmp(Hour(0.99999999))
    s = s & Dmp(Minute(0.5 / 24 / 60 * 59.9))
    s = s & Dmp(Second(0.5))
    s = s & Dmp(Year(-1))
    s = s & Dmp(Day(60))
    s = s & Dmp(Month(59))
    s = s & Dmp(Weekday(-1))
    s = s & Dmp(Year(Null))
    s = s & ErrS()
    s = s & Dmp(Year(""))
    s = s & ErrS()
    s = s & Dmp(Year(True))
    s = s & Dmp(Year("x"))
    s = s & ErrS()
    s = s & Dmp(Hour(Empty))
    R = s
Done:
    M14 = R
    Exit Function
EH:
    M14 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M15() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Variant
    a = Split("a,b,,c,", ",")
    s = Dmp(a)
    s = s & Dmp(Split("", ","))
    s = s & UBound(Split("", ","))
    s = s & Dmp(Split("abc", ""))
    s = s & Dmp(Split("a  b", " "))
    s = s & Dmp(Split("a,b,c", ",", 0))
    s = s & ErrS()
    On Error Resume Next
    s = s & Dmp(Split("a,b,c", ",", -2))
    s = s & ErrS()
    s = s & Dmp(Split(Null))
    s = s & ErrS()
    s = s & Dmp(Split("a b"))
    s = s & Dmp(Split("aXbxc", "x", , vbTextCompare))
    s = s & Dmp(Split(5.5, "."))
    s = s & Dmp(Split("a,b", Null))
    s = s & ErrS()
    R = s
Done:
    M15 = R
    Exit Function
EH:
    M15 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M16() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Hx(Join(Array(1, "a", Null, Empty, 2.5, True), "-"))
    s = s & ErrS()
    s = s & Hx(Join(Array(), ","))
    s = s & ErrS()
    s = s & Hx(Join(Array("a", "b"), ""))
    s = s & Hx(Join(Array("a", "b")))
    s = s & Hx(Join(Array(1, 2), Null))
    s = s & ErrS()
    s = s & Hx(Join("abc"))
    s = s & ErrS()
    s = s & Hx(Join(Array(Array(1), 2), ","))& ErrS()
    s = s & Hx(Join(Array(#1/2/2000#, CCur(1.5)), ","))
    R = s
Done:
    M16 = R
    Exit Function
EH:
    M16 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M17() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Variant
    a = Array("Apple", "apple", "Banana", "applesauce", "", Null)
    On Error Resume Next
    s = Dmp(Filter(a, "app"))
    s = s & ErrS()
    s = s & Dmp(Filter(Array("Apple", "apple", "Banana", "applesauce"), "app", True, vbTextCompare))
    s = s & Dmp(Filter(Array("Apple", "apple", "Banana"), "app", False))
    s = s & Dmp(Filter(Array("a", "b"), ""))
    s = s & Dmp(Filter(Array("a", "b"), "z"))
    s = s & Dmp(UBound(Filter(Array("a", "b"), "z")))
    s = s & Dmp(Filter(Array(1, 22, 3), 2))
    s = s & Dmp(Filter(Array("a", "b"), Null))
    s = s & ErrS()
    s = s & Dmp(Filter("abc", "a"))
    s = s & ErrS()
    R = s
Done:
    M17 = R
    Exit Function
EH:
    M17 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M18() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Dmp(IIf(True, 1, 1 / 0))
    s = s & ErrS()
    s = s & Dmp(Choose(0, 1, 2))
    s = s & Dmp(Choose(3, 1, 2))
    s = s & Dmp(Choose(1.5, "a", "b", "c"))
    s = s & Dmp(Choose(2.5, "a", "b", "c"))
    s = s & Dmp(Choose(Null, 1))
    s = s & ErrS()
    s = s & Dmp(Switch(False, 1, True, 2, True, 3))
    s = s & Dmp(Switch(False, 1))
    s = s & ErrS()
    s = s & Dmp(Switch(1))
    s = s & ErrS()
    R = s
Done:
    M18 = R
    Exit Function
EH:
    M18 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M19() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Err.Clear
    s = "a"
    s = s & Err.Number
    Err.Raise 5: s = s & "b" & Err.Number & Err.Description
    Err.Raise 5, , "x": s = s & Err.Description & Err.Source & "|"
    Err.Clear
    Err.Raise 0: s = s & "c" & Err.Number & Err.Description & "|"
    Err.Clear
    Err.Raise 65536: s = s & "d" & Err.Number & Err.Description & "|"
    Err.Clear
    Err.Raise -1: s = s & "e" & Err.Number & Err.Description & "|"
    Err.Clear
    Err.Raise 513, "S", "txt", "h", 5: s = s & Err.Number & Err.Description & Err.Source & Err.HelpFile & Err.HelpContext & "|"
    Err.Clear
    Err.Raise vbObjectError + 1: s = s & Err.Number & Err.Description & Err.Source & "|"
    Err.Clear
    Err.Raise 1000: s = s & Err.Number & Err.Description & Err.Source & "|"
    Err.Clear
    Err.Raise 91: s = s & Err.Number & Err.Description & "|"
    Err.Clear
    Err.Raise 32767: s = s & Err.Number & Err.Description & "|"
    Err.Raise 65535: s = s & Err.Number & Err.Description & "|"
    Err.Raise 1.5: s = s & Err.Number & Err.Description & "|"
    Err.Raise "7": s = s & Err.Number & Err.Description & "|"
    Err.Raise "x": s = s & Err.Number & Err.Description & "|"
    R = s
Done:
    M19 = R
    Exit Function
EH:
    M19 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M20() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error Resume Next
    For i = 1 To 20
        Err.Clear
        Err.Raise Choose(i, 3, 6, 7, 9, 10, 11, 13, 14, 16, 17, 18, 20, 28, 35, 47, 48, 49, 51, 52, 53)
        s = s & Err.Number
        s = s & "="
        s = s & Err.Description
        s = s & ";"
    Next i
    R = s
Done:
    M20 = R
    Exit Function
EH:
    M20 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M21() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error Resume Next
    For i = 1 To 20
        Err.Clear
        Err.Raise Choose(i, 54, 55, 57, 58, 59, 61, 62, 63, 67, 68, 70, 71, 74, 75, 76, 92, 93, 94, 97, 322)
        s = s & Err.Number
        s = s & "="
        s = s & Err.Description
        s = s & ";"
    Next i
    R = s
Done:
    M21 = R
    Exit Function
EH:
    M21 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M22() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error Resume Next
    For i = 1 To 20
        Err.Clear
        Err.Raise Choose(i, 380, 381, 382, 383, 385, 387, 400, 423, 424, 425, 429, 430, 432, 438, 440, 445, 446, 447, 448, 449)
        s = s & Err.Number
        s = s & "="
        s = s & Err.Description
        s = s & ";"
    Next i
    R = s
Done:
    M22 = R
    Exit Function
EH:
    M22 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M23() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error Resume Next
    For i = 1 To 20
        Err.Clear
        Err.Raise Choose(i, 450, 451, 452, 453, 454, 455, 457, 458, 459, 460, 461, 462, 463, 481, 482, 483, 484, 485, 486, 1004)
        s = s & Err.Number
        s = s & "="
        s = s & Err.Description
        s = s & ";"
    Next i
    R = s
Done:
    M23 = R
    Exit Function
EH:
    M23 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M24() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error Resume Next
    For i = 1 To 20
        Err.Clear
        Err.Raise Choose(i, 2, 4, 8, 12, 15, 19, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 36)
        s = s & Err.Number
        s = s & "="
        s = s & Err.Description
        s = s & ";"
    Next i
    R = s
Done:
    M24 = R
    Exit Function
EH:
    M24 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M25() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, e As Long
    On Error Resume Next
    For i = 1 To 12
        Err.Clear
        Select Case i
        Case 1: e = CInt("x")
        Case 2: e = 1 / 0
        Case 3: e = CLng(2147483648#)
        Case 4: e = CByte(256)
        Case 5: Dim o As Object: o.Foo
        Case 6: e = Chr(300)
        Case 7: e = Mid("a", 0)
        Case 8: e = Left("a", -1)
        Case 9: e = Space(-1)
        Case 10: e = String(-1, "a")
        Case 11: e = Sqr(-1)
        Case 12: e = Log(0)
        End Select
        s = s & Err.Number
        s = s & "="
        s = s & Err.Description
        s = s & ";"
    Next i
    R = s
Done:
    M25 = R
    Exit Function
EH:
    M25 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M26() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, e As Variant, a(1 To 3) As Long, c As New Collection, d As Date
    On Error Resume Next
    For i = 1 To 14
        Err.Clear
        Select Case i
        Case 1: e = a(4)
        Case 2: e = c(1)
        Case 3: e = c("k")
        Case 4: c.Add 1, "k": c.Add 2, "k"
        Case 5: e = Array(1)(5)
        Case 6: Erase a: e = a(1)
        Case 7: e = CDate("x")
        Case 8: e = CVErr(5) + 1
        Case 9: e = Null + 1: e = CLng(Null)
        Case 10: e = Asc("")
        Case 11: e = Hex("x")
        Case 12: e = InStr(0, "a", "a")
        Case 13: e = Mid$("abc", 2, -1)
        Case 14: e = Replace("a", "a", "b", 0)
        End Select
        s = s & Err.Number
        s = s & "="
        s = s & Err.Description
        s = s & ";"
    Next i
    R = s
Done:
    M26 = R
    Exit Function
EH:
    M26 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M27() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Dim x As Long
    x = 1 / 0
    s = Err.Number
    s = s & "|"
    x = 5
    s = s & Err.Number
    s = s & "|"
    Err.Number = 0
    On Error GoTo 0
    On Error Resume Next
    x = 1 / 0
    On Error GoTo -1
    s = s & Err.Number
    s = s & "|"
    R = s
Done:
    M27 = R
    Exit Function
EH:
    M27 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M28() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, x As Long
    On Error GoTo H
    x = 1 / 0
    s = s & "no"
    GoTo Done
H:
    s = s & "H"
    s = s & Err.Number
    On Error Resume Next
    Resume Next
    R = s
Done:
    M28 = R
    Exit Function
EH:
    M28 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M29() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, x As Long, n As Long
    On Error GoTo H
    n = n + 1
    x = 1 / 0
    s = s & "after" & n
    R = s
    GoTo Done
H:
    s = s & "H" & n & ","
    Resume Next
Done:
    M29 = R
    Exit Function
EH:
    M29 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M30() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long, x As Long
    On Error GoTo H
    n = n + 1
    x = 1 / (n - 1)
    R = "n=" & n
    GoTo Done
H:
    n = n + 10
    Resume
Done:
    M30 = R
    Exit Function
EH:
    M30 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M31() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long, x As Long
    On Error GoTo H
    x = 1 / 0
    s = "x"
L1:
    s = s & "L1"
    R = s
    GoTo Done
H:
    Resume L1
Done:
    M31 = R
    Exit Function
EH:
    M31 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M32() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long
    On Error GoTo H
    Err.Raise 5
    R = "no"
    GoTo Done
H:
    On Error GoTo H2
    Err.Raise 6
    GoTo Done
H2:
    R = "H2:" & Err.Number & "/" & n
Done:
    M32 = R
    Exit Function
EH:
    M32 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M33() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error GoTo H
    Err.Raise 5
    GoTo Done
H:
    s = Err.Number & "|"
    On Error GoTo -1
    s = s & Err.Number & "|"
    On Error GoTo 0
    Err.Raise 6
    R = s
Done:
    M33 = R
    Exit Function
EH:
    M33 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M34() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = s & InnerErr1()
    s = s & "|"
    s = s & Err.Number
    R = s
Done:
    M34 = R
    Exit Function
EH:
    M34 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M35() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    v = Array(1, 2, 3)
    ReDim Preserve v(0 To 5)
    s = Dmp(v)
    On Error Resume Next
    ReDim Preserve v(1 To 5)
    s = s & ErrS()
    Dim m() As Long
    ReDim m(1 To 2, 1 To 3)
    ReDim Preserve m(1 To 2, 1 To 4)
    s = s & UBound(m, 2)
    s = s & ErrS()
    ReDim Preserve m(1 To 3, 1 To 4)
    s = s & ErrS()
    Erase m
    s = s & UBound(m)
    s = s & ErrS()
    R = s
Done:
    M35 = R
    Exit Function
EH:
    M35 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M36() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a() As Variant, b(1 To 3) As Long
    On Error Resume Next
    s = s & UBound(a)
    s = s & ErrS()
    s = s & LBound(a)
    s = s & ErrS()
    s = s & Dmp(IsArray(a))
    s = s & Dmp(Array())
    s = s & UBound(Array())
    s = s & LBound(Array())
    a = Array()
    s = s & UBound(a)
    s = s & LBound(a)
    s = s & Dmp(b)
    s = s & UBound(b, 2)
    s = s & ErrS()
    s = s & UBound(b, 0)
    s = s & ErrS()
    s = s & UBound(b, 1)
    Erase b
    s = s & Dmp(b)
    R = s
Done:
    M36 = R
    Exit Function
EH:
    M36 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M37() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Variant, b As Variant
    a = Array(1, Array(2, 3), "x")
    b = a
    b(0) = 9
    s = Dmp(a(0)) & Dmp(b(0)) & Dmp(a(1)(1)) & TypeName(a) & TypeName(a(1)) & VarType(a) & VarType(Array(1)) & TypeName(Array(1)(0))
    Dim l() As Long
    ReDim l(0 To 1)
    s = s & TypeName(l) & VarType(l) & "|" & TypeName(l(0))
    R = s
Done:
    M37 = R
    Exit Function
EH:
    M37 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M38() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, c As New Collection
    c.Add "a": c.Add "b", "kb": c.Add "c", , 1: c.Add "d", , , 2
    s = c.Count
    s = s & c(1)
    s = s & c(2)
    s = s & c(3)
    s = s & c(4)
    s = s & c("kb")
    On Error Resume Next
    c.Add "e", "KB": s = s & ErrS()
    c.Add "f", "q", 1, 2: s = s & ErrS()
    c.Add "g", , 10: s = s & ErrS()
    c.Remove 10: s = s & ErrS()
    c.Remove "zz": s = s & ErrS()
    s = s & c("KB")
    s = s & c(1.5)
    s = s & ErrS()
    s = s & c(0)
    s = s & ErrS()
    s = s & c("")
    s = s & ErrS()
    c.Remove 1
    s = s & c.Count
    s = s & c(1)
    R = s
Done:
    M38 = R
    Exit Function
EH:
    M38 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M39() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, c As New Collection, v As Variant
    c.Add 1: c.Add 2: c.Add 3
    For Each v In c
        If v = 1 Then c.Add 4
        If v = 2 Then c.Remove 3
        s = s & v & ","
    Next v
    s = s & c.Count
    R = s
Done:
    M39 = R
    Exit Function
EH:
    M39 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M40() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, d As Object
    Set d = CreateObject("Scripting.Dictionary")
    d.Add "a", 1: d("b") = 2: d("A") = 3
    s = d.Count
    s = s & d("a")
    s = s & d("A")
    s = s & d.Exists("B")
    s = s & d.CompareMode
    On Error Resume Next
    d.Add "a", 5: s = s & ErrS()
    s = s & d("zz")
    s = s & d.Count
    s = s & ErrS()
    d.Remove "nope": s = s & ErrS()
    d.CompareMode = 1: s = s & ErrS()
    d.RemoveAll
    d.CompareMode = 1
    d.Add "a", 1: d.Add "A", 2
    s = s & d.Count
    s = s & Dmp(d.Keys)
    s = s & Dmp(d.Items)
    s = s & Dmp(d.Items())
    s = s & d.Item("A")
    d.Key("a") = "z": s = s & Dmp(d.Keys) & ErrS()
    d(1) = "num": d(1#) = "dbl": d("1") = "str": s = s & d.Count
    d(Null) = 1: s = s & ErrS()
    d(Empty) = 1: s = s & d.Count & ErrS()
    R = s
Done:
    M40 = R
    Exit Function
EH:
    M40 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M41() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, d As Object, k As Variant
    Set d = CreateObject("Scripting.Dictionary")
    d.CompareMode = 1
    d("b") = 1: d("a") = 2: d("c") = 3
    For Each k In d
        s = s & k
        s = s & d(k)
        s = s & ","
    Next k
    s = s & TypeName(d)
    s = s & TypeName(d.Keys)
    s = s & TypeName(d.Items(0))
    s = s & ErrS()
    On Error Resume Next
    s = s & d.Items(1)
    s = s & ErrS()
    R = s
Done:
    M41 = R
    Exit Function
EH:
    M41 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M42() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range, c As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:C3").Formula = "=ROW()*10+COLUMN()"
    ws.Range("B2").ClearContents
    For Each c In ws.Range("A1:C3")
        s = s & c.Address(0, 0) & c.Value & ","
    Next c
    For Each c In ws.Range("A1:B2,C3")
        s = s & c.Address(0, 0)
    Next c
    For Each c In ws.Range("A1:B2").Rows
        s = s & c.Address(0, 0)
    Next c
    For Each c In ws.Range("A1:B2").Columns
        s = s & c.Address(0, 0)
    Next c
    Dim v As Variant
    For Each v In ws.Range("A1:B2").Value
        s = s & Dmp(v)
    Next v
    For Each v In ws.Range("A1:B2")
        s = s & TypeName(v)
    Next v
    R = s
Done:
    M42 = R
    Exit Function
EH:
    M42 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M43() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, a As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:C3").Value = 5
    ws.Range("B2").Value = Empty
    For Each a In ws.Range("A1:B2,C1:C3,A3").Areas
        s = s & a.Address(0, 0)
        s = s & ";"
    Next a
    s = s & ws.Range("A1:B2,C1:C3").Cells.Count
    s = s & ws.Range("A1:B2,C1:C3").Rows.Count
    s = s & ws.Range("A1:B2,C1:C3").Columns.Count
    s = s & ws.Range("A1:B2,C1:C3").Cells(3).Address
    On Error Resume Next
    s = s & ws.Range("A1:B2,C1:C3").Cells(5).Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2,C1:C3").Cells(9).Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2,C1:C3").Rows(2).Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2,C1:C3").Columns(3).Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2,C1:C3").Value
    s = s & ErrS()
    s = s & Dmp(ws.Range("A1:B2,C1:C3").Value2)
    s = s & ErrS()
    R = s
Done:
    M43 = R
    Exit Function
EH:
    M43 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M44() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, v As Variant
    For Each v In Array(5, 15, 50, 100, 0, -1, 1000, 3.5, "abc", "x", "", Null, Empty, True)
        Select Case v
        Case 1 To 10: s = s & "A"
        Case 11 To 20, 50: s = s & "B"
        Case Is > 99, Is < 0: s = s & "C"
        Case "a" To "b": s = s & "D"
        Case "x", "y": s = s & "E"
        Case Is = 0: s = s & "F"
        Case Else: s = s & "Z"
        End Select
    Next v
    R = s
Done:
    M44 = R
    Exit Function
EH:
    M44 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M45() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    On Error Resume Next
    For Each v In Array(5, "5", "05", 5.5, "abc", "B", "b", "")
        Select Case v
        Case "5": s = s & "s5"
        Case 5: s = s & "n5"
        Case "A" To "C": s = s & "r"
        Case 6 To 4: s = s & "rev"
        End Select
        s = s & ErrS()
        s = s & ","
    Next v
    R = s
Done:
    M45 = R
    Exit Function
EH:
    M45 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M46() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Variant
    For Each a In Array("abc", "ABC", "a1c", "a-c", "a*c", "a?c", "")
        s = s & -(a Like "a[a-z]c") & -(a Like "a[!a-z]c") & -(a Like "a[*]c") & -(a Like "a[?]c") & -(a Like "?*") & -(a Like "[a-c][a-c][a-c]") & -(a Like "A[B-C]*") & -(a Like "a#c") & -(a Like "") & ","
    Next a
    R = s
Done:
    M46 = R
    Exit Function
EH:
    M46 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M47() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Opt() & "|" & Opt(1) & "|" & Opt(, 3) & "|" & Opt(Empty, , "z") & "|" & Opt(Null) & "|" & Opt(b:=9) & "|" & Opt(c:="q", a:=2)
    R = s
Done:
    M47 = R
    Exit Function
EH:
    M47 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M48() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = PA() & PA(1) & PA(1, "a", Null) & PA(Array(1, 2)) & PA(Empty, Null)
    R = s
Done:
    M48 = R
    Exit Function
EH:
    M48 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M49() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = PA(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
    R = s
Done:
    M49 = R
    Exit Function
EH:
    M49 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M50() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Variant, y As Long, s As String, z As Double
    x = 1: y = 1: z = 1.5
    Bump x: BumpL y
    Bump (x)
    BumpL (y)
    s = x
    s = s & ","
    s = s & y
    BumpL CLng(y)
    s = s & ","
    s = s & y
    On Error Resume Next
    s = s & ErrS()
    R = s
Done:
    M50 = R
    Exit Function
EH:
    M50 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M51() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Variant
    i = 1
    s = Dmp(i + "1")
    s = s & Dmp(i & "1")
    s = s & Dmp("1" + "1")
    s = s & Dmp("1" + 1)
    s = s & Dmp("a" + "b")
    s = s & Dmp(i + Null)
    s = s & Dmp(Null & "a")
    s = s & Dmp(Empty + Empty)
    s = s & Dmp(Empty & Empty)
    s = s & Dmp(True + True)
    s = s & Dmp(True & True)
    s = s & Dmp(-True)
    s = s & Dmp(Not 5)
    s = s & Dmp(Not Null)
    s = s & Dmp(5 And 3.7)
    s = s & Dmp(5 Or "3")
    s = s & Dmp(Null And False)
    s = s & Dmp(Null Or True)
    s = s & Dmp(Null Xor True)
    s = s & Dmp(Null Eqv Null)
    s = s & Dmp(5 Imp 3)
    s = s & Dmp(#1/1/2000# + 1)
    s = s & Dmp(#1/1/2000# & 1)
    s = s & Dmp(#1/1/2000# - #1/2/2000#)
    On Error Resume Next
    s = s & Dmp("a" + 1)
    s = s & ErrS()
    s = s & Dmp("" + 1)
    s = s & ErrS()
    s = s & Dmp(Null + "a")
    s = s & Dmp("1" - "a")
    s = s & ErrS()
    s = s & Dmp(Empty - "")
    s = s & ErrS()
    s = s & Dmp(5 And "x")
    s = s & ErrS()
    s = s & Dmp(CVErr(3) & "a")
    s = s & ErrS()
    R = s
Done:
    M51 = R
    Exit Function
EH:
    M51 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M52() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Variant
    On Error Resume Next
    s = Dmp(7 \ 2)
    s = s & Dmp(-7 \ 2)
    s = s & Dmp(7 \ -2)
    s = s & Dmp(7.5 \ 2.5)
    s = s & Dmp(8.5 \ 2.5)
    s = s & Dmp(7 Mod 3)
    s = s & Dmp(-7 Mod 3)
    s = s & Dmp(7 Mod -3)
    s = s & Dmp(7.5 Mod 2)
    s = s & Dmp(8.5 Mod 2)
    s = s & Dmp(2.5 Mod 1)
    s = s & Dmp(3.5 Mod 1)
    s = s & Dmp(2147483648# Mod 3)
    s = s & ErrS()
    s = s & Dmp(5 \ 0)
    s = s & ErrS()
    s = s & Dmp(5 Mod 0)
    s = s & ErrS()
    s = s & Dmp(5 Mod 0.4)
    s = s & ErrS()
    s = s & Dmp(5 \ 0.4)
    s = s & ErrS()
    s = s & Dmp(2 ^ -1)
    s = s & Dmp(0 ^ 0)
    s = s & Dmp(0 ^ -1)
    s = s & ErrS()
    s = s & Dmp((-8) ^ (1 / 3))
    s = s & ErrS()
    s = s & Dmp(-2 ^ 2)
    s = s & Dmp(2 ^ 3 ^ 2)
    s = s & Dmp(-2 ^ -2)
    s = s & Dmp(1E+308 * 10)
    s = s & ErrS()
    s = s & Dmp(CInt(32767) + CInt(1))
    s = s & ErrS()
    s = s & Dmp(CByte(255) + CByte(1))
    s = s & ErrS()
    s = s & Dmp(CInt(200) * CInt(200))
    s = s & ErrS()
    s = s & Dmp(32767 + 1)
    s = s & Dmp(-32768 - 1)
    s = s & Dmp(2147483647 + 1)
    s = s & Dmp(2147483647 + 1&)
    s = s & ErrS()
    R = s
Done:
    M52 = R
    Exit Function
EH:
    M52 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M53() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, c As Currency, d As Variant
    c = 922337203685477.5807@
    s = Dmp(c)
    s = s & Dmp(c / 3)
    s = s & Dmp(c * 1)
    s = s & Dmp(CCur(0.00005))
    s = s & Dmp(CCur(0.00015))
    s = s & Dmp(CCur(0.00025))
    s = s & Dmp(CCur(-0.00005))
    s = s & Dmp(CCur(1.23456789))
    s = s & Dmp(CCur(1) / CCur(3))
    s = s & Dmp(CCur(1) / 3)
    s = s & Dmp(CCur(2) * 0.00007)
    s = s & Dmp(CCur(5) ^ 2)
    s = s & Dmp(Int(CCur(-2.5)))
    s = s & Dmp(Round(CCur(2.5)))
    s = s & Dmp(Round(CCur(1.005), 2))
    s = s & Dmp(Sgn(CCur(-1)))
    s = s & Dmp(CCur(5) Mod 3)
    s = s & Dmp(CCur(5) \ 2)
    s = s & Dmp(-CCur(5))
    s = s & Dmp(CCur("1,234.5678"))
    On Error Resume Next
    s = s & Dmp(c + 1)
    s = s & ErrS()
    s = s & Dmp(c * 2)
    s = s & ErrS()
    s = s & Dmp(CCur(922337203685478#))
    s = s & ErrS()
    s = s & Dmp(CCur(1E+15))
    s = s & ErrS()
    R = s
Done:
    M53 = R
    Exit Function
EH:
    M53 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M54() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, d As Variant
    d = CDec("79228162514264337593543950335")
    s = Dmp(d)
    s = s & Dmp(CDec(1) / CDec(3))
    s = s & Dmp(CDec(0.1) + CDec(0.2))
    s = s & Dmp(CDec(1.5) * CDec(2))
    s = s & Dmp(CDec(10) ^ 2)
    s = s & Dmp(CDec(-5) Mod 3)
    s = s & Dmp(CDec(7) \ 2)
    s = s & Dmp(Int(CDec(-1.5)))
    s = s & Dmp(Fix(CDec(-1.5)))
    s = s & Dmp(Round(CDec(1.255), 2))
    s = s & Dmp(Abs(CDec(-3)))
    s = s & Dmp(Sgn(CDec(-3)))
    s = s & Dmp(CDec(1) / 7 * 7)
    s = s & Dmp(CDec("1E-28"))
    s = s & Dmp(CDec("1E-29"))
    s = s & Dmp(CDec(1.0000000000000000000000000001))
    s = s & Dmp(CStr(CDec(1.50)))
    s = s & Dmp(CDec(0.5) + 1)
    s = s & Dmp(CDec(2) * CCur(3))
    s = s & Dmp(CDec(1) + 1.5)
    s = s & Dmp(CDec(1) + CSng(1.1))
    On Error Resume Next
    s = s & Dmp(d + 1)
    s = s & ErrS()
    s = s & Dmp(d * 2)
    s = s & ErrS()
    s = s & Dmp(CDec(1E+29))
    s = s & ErrS()
    s = s & Dmp(CDec("x"))
    s = s & ErrS()
    s = s & Dmp(CDec(Null))
    s = s & ErrS()
    s = s & Dmp(CDec(1) / 0)
    s = s & ErrS()
    s = s & Dmp(Sqr(CDec(4)))
    s = s & Dmp(CDec(4) ^ 0.5)
    s = s & Dmp(CDec(True))
    s = s & Dmp(CDec(#1/1/2000#))
    R = s
Done:
    M54 = R
    Exit Function
EH:
    M54 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M55() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(2.5, 3.5, -2.5, 0.5, 1.5, 2.675, 1.005, -0.5, 2147483647.5, 32767.5, 255.5, 0.49999999999999994)
        s = s & CInt(0)
        s = s & Round(v)
        s = s & "/"
        s = s & Round(v, 2)
        s = s & "/"
        s = s & Round(v, -1)
        s = s & "/"
        s = s & Format(v, "0")
        s = s & "/"
        s = s & Format(v, "0.00")
        s = s & "/"
        s = s & Int(v)
        s = s & Fix(v)
        s = s & ";"
    Next v
    On Error Resume Next
    s = s & CInt(32767.5)
    s = s & ErrS()
    s = s & CInt(32768)
    s = s & ErrS()
    s = s & CLng(2147483647.5)
    s = s & ErrS()
    s = s & CByte(255.5)
    s = s & ErrS()
    s = s & CByte(-0.5)
    s = s & ErrS()
    s = s & CByte(0.5)
    s = s & CByte(1.5)
    s = s & Round(2.5, 0)
    s = s & Round(1234.5678, -2)
    s = s & Round(5, 400)
    s = s & ErrS()
    s = s & Round(5, 0.5)
    s = s & ErrS()
    s = s & Round(1, "a")
    s = s & ErrS()
    R = s
Done:
    M55 = R
    Exit Function
EH:
    M55 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M56() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    On Error Resume Next
    For Each v In Array("", " ", "a", "abc", "ABC", ChrW(233), ChrW(8364), ChrW(55357), vbCr)
        s = s & Asc(v)
        s = s & ErrS()
        s = s & AscW(v)
        s = s & ErrS()
        s = s & AscB(v)
        s = s & ErrS()
        s = s & Len(v)
        s = s & LenB(v)
        s = s & Hx(StrConv(v, vbFromUnicode))
        s = s & Hx(StrConv(v, vbWide))
        s = s & Hx(StrConv(v, vbNarrow))
        s = s & ";"
    Next v
    s = s & Hx(Chr(65))
    s = s & Hx(Chr(256))
    s = s & ErrS()
    s = s & Hx(Chr(-1))
    s = s & ErrS()
    s = s & Hx(ChrW(65536))
    s = s & ErrS()
    s = s & Hx(ChrW(-1))
    s = s & Hx(ChrW(-32768))
    s = s & Hx(ChrB(65))
    s = s & Hx(Chr(128))
    s = s & Hx(Chr(255))
    s = s & Hx(ChrW(128))
    s = s & Hx(Chr(0))
    s = s & Hx(Chr$(65))
    s = s & Hx(ChrW(65535))
    s = s & ErrS()
    R = s
Done:
    M56 = R
    Exit Function
EH:
    M56 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M57() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String
    t = "Hello, World"
    s = Hx(Mid(t, 0))
    s = s & ErrS()
    s = s & Hx(Mid(t, 8))
    s = s & Hx(Mid(t, 8, 100))
    s = s & Hx(Mid(t, 13))
    s = s & Hx(Mid(t, 14))
    s = s & Hx(Mid(t, 1, 0))
    s = s & Hx(Left(t, 0))
    s = s & Hx(Left(t, 100))
    s = s & Hx(Right(t, 100))
    s = s & Hx(Right(t, 0))
    s = s & Hx(Mid(t, 2.5, 2.5))
    s = s & Hx(Mid(t, 3.5, 1.5))
    s = s & Hx(Left(t, 2.5))
    s = s & Hx(Left(t, 3.5))
    s = s & Hx(Left(Null, 1))
    s = s & Hx(Left(12345, 2))
    s = s & Hx(Mid(Empty, 1))
    s = s & Hx(Left("abc", True))
    On Error Resume Next
    s = s & Hx(Mid(t, 1, -1))
    s = s & ErrS()
    s = s & Hx(Mid(t, -1))
    s = s & ErrS()
    s = s & Hx(Left(t, -1))
    s = s & ErrS()
    s = s & Hx(Mid(Null, 1))
    s = s & ErrS()
    s = s & Hx(Left(t, Null))
    s = s & ErrS()
    s = s & Hx(Mid(t, 2147483648#))
    s = s & ErrS()
    Mid(t, 1, 2) = "XYZ": s = s & t
    Mid(t, 11) = "123456": s = s & t
    Mid(t, 20) = "q": s = s & ErrS()
    Mid(t, 0) = "q": s = s & ErrS()
    Mid(t, 1) = "": s = s & t
    R = s
Done:
    M57 = R
    Exit Function
EH:
    M57 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M58() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = InStr("abcabc", "c")
    s = s & InStr(4, "abcabc", "c")
    s = s & InStr(7, "abcabc", "c")
    s = s & InStr("abc", "")
    s = s & InStr(2, "abc", "")
    s = s & InStr(4, "abc", "")
    s = s & InStr("", "a")
    s = s & InStr("", "")
    s = s & InStr(1, "ABC", "b", vbTextCompare)
    s = s & InStr(Null, "a")
    s = s & InStr(1, Null, "a")
    s = s & InStr("abc", Null)
    s = s & "|"
    s = s & InStrRev("abcabc", "c")
    s = s & InStrRev("abcabc", "c", 5)
    s = s & InStrRev("abcabc", "c", 0 + 6)
    s = s & InStrRev("abc", "")
    s = s & InStrRev("abc", "", 1)
    s = s & InStrRev("abc", "c", -1)
    s = s & InStrRev("ABC", "b", -1, vbTextCompare)
    s = s & InStrRev("abc", "a", 100)
    On Error Resume Next
    s = s & InStr(0, "a", "a")
    s = s & ErrS()
    s = s & InStr(-1, "a", "a")
    s = s & ErrS()
    s = s & InStrRev("abc", "a", 0)
    s = s & ErrS()
    s = s & InStrRev("abc", "a", -2)
    s = s & ErrS()
    s = s & InStrRev(Null, "a")
    s = s & InStr("a", "a", 1)
    s = s & ErrS()
    s = s & InStr(1, "a", "a", 9)
    s = s & ErrS()
    R = s
Done:
    M58 = R
    Exit Function
EH:
    M58 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M59() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(Replace("aaa", "a", "b", 2))
    s = s & Hx(Replace("aaa", "a", "b", 2, 1))
    s = s & Hx(Replace("aaa", "a", "b", 4))
    s = s & Hx(Replace("aaa", "", "b"))
    s = s & Hx(Replace("abc", "b", ""))
    s = s & Hx(Replace("", "a", "b"))
    s = s & Hx(Replace("abc", "B", "x", , , vbTextCompare))
    s = s & Hx(Replace("aaa", "aa", "a"))
    s = s & Hx(Replace("abc", "c", "cc", 1, 0))
    s = s & Hx(Replace("abc", "c", "x", -1))
    s = s & ErrS()
    On Error Resume Next
    s = s & Hx(Replace("abc", "c", "x", 0))
    s = s & ErrS()
    s = s & Hx(Replace("abc", "c", "x", 1, -2))
    s = s & ErrS()
    s = s & Hx(Replace(Null, "a", "b"))
    s = s & ErrS()
    s = s & Hx(Replace("a", Null, "b"))
    s = s & ErrS()
    s = s & Hx(Replace("a", "a", Null))
    s = s & ErrS()
    s = s & Hx(Replace(123, 2, 9))
    s = s & Hx(Replace("a", "a", "b", 1.5, 1.5))
    R = s
Done:
    M59 = R
    Exit Function
EH:
    M59 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M60() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(UCase("stra" & ChrW(223) & "e")) & Hx(LCase(ChrW(304))) & Hx(UCase(ChrW(223))) & Hx(StrConv("hello wORLD o'neil", vbProperCase)) & Hx(StrConv(ChrW(454), vbUpperCase)) & Hx(Trim(vbTab & " a " & vbTab)) & Hx(Trim(ChrW(160) & "a" & ChrW(160))) & Hx(LTrim("  a ")) & Hx(RTrim(" a  ")) & Hx(Space(3)) & Hx(String(3, "ab")) & Hx(String(2, 65)) & Hx(String(2, "")) & ErrS() & Hx(String(0, "a")) & Hx(StrReverse("abc")) & Hx(StrReverse("")) & Hx(LCase(Null)) & ErrS()
    R = s
Done:
    M60 = R
    Exit Function
EH:
    M60 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M61() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    Dim v As Variant
    On Error Resume Next
    For Each v In Array(Null, Empty, 123.45, True, #1/1/2000#, "abc", CCur(1.5), CByte(1), 1&, 1!)
        s = s & Dmp(Len(v))
        s = s & ErrS()
        s = s & Dmp(LenB(v))
        s = s & ErrS()
    Next v
    R = s
Done:
    M61 = R
    Exit Function
EH:
    M61 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M62() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(5, "5", 5.5, True, Null, Empty, "", "a", #1/1/2000#, CCur(5), Array(1), CVErr(7), Nothing)
        s = s & TypeName(v) & VarType(v) & IsNumeric(v) & IsDate(v) & IsEmpty(v) & IsNull(v) & IsError(v) & IsObject(v) & IsArray(v) & ";"
    Next v
    R = s
Done:
    M62 = R
    Exit Function
EH:
    M62 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M63() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array("1", "1.5", " 1 ", "1e5", "&H10", "$5", "1,000", "(1)", "1-", "-1", "+1", "", "1/2/2000", "12:30", "5%", "1d5", "0x1", "1 1", ChrW(1633))
        s = s & IsNumeric(v) & IsDate(v) & ";"
    Next v
    R = s
Done:
    M63 = R
    Exit Function
EH:
    M63 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M64() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(2147483647, 2147483648#, -2147483648#, 32767, 32768, 255, 256, -1, 1.5, 1E+10, 1E+20)
        s = s & TypeName(v) & ",": s = s & TypeName(v + 0) & TypeName(v * 1) & TypeName(v / 1) & TypeName(v \ 1) & TypeName(v ^ 1) & TypeName(-v) & TypeName(v Mod 1000) & ";"
    Next v
    R = s
Done:
    M64 = R
    Exit Function
EH:
    M64 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M65() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = TypeName(1) & TypeName(1&) & TypeName(1!) & TypeName(1#) & TypeName(1@) & TypeName(&H7FFF) & TypeName(&H8000) & TypeName(&HFFFF) & TypeName(&H10000) & TypeName(&HFFFFFFFF) & TypeName(&H7FFFFFFF) & TypeName(&O7) & TypeName(1E5) & TypeName(100000) & TypeName(2147483648#) & TypeName(-32768) & TypeName(-32769) & TypeName(1.5E+30) & TypeName(5 / 1) & TypeName(5 \ 1) & TypeName(True) & TypeName(#1/1/2000#) & TypeName("a") & TypeName(Nothing) & TypeName(Null) & TypeName(Empty) & TypeName(CVErr(1)) & TypeName(&H8000&) & TypeName(&HFFFF&) & TypeName(&H80000000)
    R = s
Done:
    M65 = R
    Exit Function
EH:
    M65 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M66() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(&H8000, &HFFFF, &HFFFF&, &H80000000, &HFFFFFFFF, &H7FFF)
        s = s & v & ","
    Next v
    R = s
Done:
    M66 = R
    Exit Function
EH:
    M66 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M67() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Dmp(CInt("1e2"))
    s = s & Dmp(CLng(" 12 "))
    s = s & Dmp(CLng("1,2"))
    s = s & Dmp(CLng("&H10"))
    s = s & Dmp(CLng("&HFFFF"))
    s = s & Dmp(CLng("&HFFFFFFFF"))
    s = s & Dmp(CInt("&HFFFF"))
    s = s & Dmp(CInt("&H8000"))
    s = s & Dmp(CInt("&H10000"))
    s = s & ErrS()
    s = s & Dmp(CDbl("1,5"))
    s = s & Dmp(CDbl("1e400"))
    s = s & ErrS()
    s = s & Dmp(CDbl(" "))
    s = s & ErrS()
    s = s & Dmp(CDbl("1 000"))
    s = s & ErrS()
    s = s & Dmp(CBool("true"))
    s = s & Dmp(CBool("TRUE"))
    s = s & Dmp(CBool("1"))
    s = s & Dmp(CBool("yes"))
    s = s & ErrS()
    s = s & Dmp(CBool("0"))
    s = s & Dmp(CBool(0.0001))
    s = s & Dmp(CBool(""))
    s = s & ErrS()
    s = s & Dmp(CBool(Null))
    s = s & ErrS()
    s = s & Dmp(CBool(Empty))
    s = s & Dmp(CSng("1e39"))
    s = s & ErrS()
    s = s & Dmp(CSng(3.4E+38))
    s = s & Dmp(CSng(1E-46))
    s = s & Dmp(CStr(Null))
    s = s & ErrS()
    s = s & Dmp(CStr(Empty))
    s = s & Dmp(CStr(True))
    s = s & Dmp(CStr(CVErr(7)))
    s = s & ErrS()
    R = s
Done:
    M67 = R
    Exit Function
EH:
    M67 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M68() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(0.1, 1 / 3, 100, 1E+15, 1E+16, 123456789012345680#, 1E-4, 0.00001234, 1.5E-10, 12345.6789012345, 1E+100, -1E-100, 2 ^ 53, 2 ^ 63, 1 / 7)
        s = s & CStr(v) & "|" & CSng(v) & "|" & Format(v) & "|" & Format(v, "General Number") & "|" & v & ";"
    Next v
    R = s
Done:
    M68 = R
    Exit Function
EH:
    M68 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M69() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(0, 1, -1, 1234.5, 0.5, 1E+10, 0.00001, 123456789.123456789)
        s = s & Format(v, "0.0#") & "|" & Format(v, "#,##0.00") & "|" & Format(v, "0.00E+00") & "|" & Format(v, "00000.000") & "|" & Format(v, "#.#") & "|" & Format(v, "0%") & "|" & Format(v, "$#,##0;($#,##0);""zero""") & "|" & Format(v, "@") & "|" & Format(v, "\x0") & "|" & Format(v, "0.0,") & "|" & Format(v, "#,##0,") & "|" & Format(v, "0.00;;;""n""") & "|" & Format(v, "#.00") & "|" & Format(v, ".#") & "|" & Format(v, "0\.0") & "|" & Format(v, "# ?/?") & "|" & Format(v, "*-0") & ";"
    Next v
    R = s
Done:
    M69 = R
    Exit Function
EH:
    M69 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M70() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array("abc", "", "Abc Def", "123", Null, Empty, True)
        s = s & Format(v, "@") & "|" & Format(v, "@@@@@") & "|" & Format(v, "!@@@@@") & "|" & Format(v, ">@") & "|" & Format(v, "<@") & "|" & Format(v, "&") & "|" & Format(v, "@;""empty""") & "|" & Format(v, ">") & "|" & Format(v, "x@y") & "|" & Format(v, "\a@") & "|" & Format(v, ";@") & "|" & Format(v, "0") & ";"
    Next v
    R = s
Done:
    M70 = R
    Exit Function
EH:
    M70 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M71() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(#1/2/2000 3:04:05 PM#, #12/31/1999 11:59:59 PM#, #1/1/2000#, #6:07:08 AM#, 0.5, 36526.75)
        s = s & Format(v, "yyyy-mm-dd hh:nn:ss") & "|" & Format(v, "d/m/yy h:mm AM/PM") & "|" & Format(v, "dddd mmmm") & "|" & Format(v, "ddd mmm") & "|" & Format(v, "ww w y q") & "|" & Format(v, "ttttt") & "|" & Format(v, "c") & "|" & Format(v, "m") & "|" & Format(v, "h m") & "|" & Format(v, "m h") & "|" & Format(v, "hh:mm:ss.000") & "|" & Format(v, "A/P a/p") & "|" & Format(v, "[h]:mm") & "|" & Format(v, "mmmmm") & "|" & Format(v, "oo yyyyy") & "|" & Format(v, "dddddd") & "|" & Format(v, "ddddd") & ";"
    Next v
    R = s
Done:
    M71 = R
    Exit Function
EH:
    M71 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M72() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(1, 0, -1, "a", "", Null, Empty, True, 0.5)
        s = s & Format(v, "Yes/No") & "|" & Format(v, "True/False") & "|" & Format(v, "On/Off") & "|" & Format(v, "General Number") & "|" & Format(v, "Standard") & "|" & Format(v, "Fixed") & "|" & Format(v, "Currency") & "|" & Format(v, "Percent") & "|" & Format(v, "Scientific") & "|" & Format(v) & "|" & Format(v, "") & "|" & Format(v, " ") & ";"
    Next v
    R = s
Done:
    M72 = R
    Exit Function
EH:
    M72 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M73() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1").Value = "x"
    ws.Range("A2").Formula = "=1+1"
    ws.Range("A3").Formula = "=A2&""t"""
    ws.Range("A4").Value = 1.5
    ws.Range("A5").Value = CDate("1/2/2000")
    ws.Range("A6").Value = True
    ws.Range("A7").Formula = "=NA()"
    ws.Range("A8").Value = CCur(2.5)
    ws.Range("A9").Formula = "=1/0"
    Dim i As Long
    For i = 1 To 9
        s = s & Dmp(ws.Cells(i, 1).Value) & Dmp(ws.Cells(i, 1).Value2) & Dmp(ws.Cells(i, 1).Formula) & Dmp(ws.Cells(i, 1).Text) & Dmp(ws.Cells(i, 1).NumberFormat) & ";"
    Next i
    R = s
Done:
    M73 = R
    Exit Function
EH:
    M73 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M74() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    On Error Resume Next
    ws.Range("A1").Value = "1/2/2000": s = s & Dmp(ws.Range("A1").Value2) & ws.Range("A1").NumberFormat & "|"
    ws.Range("A2").Value = "12:30": s = s & Dmp(ws.Range("A2").Value2) & ws.Range("A2").NumberFormat & "|"
    ws.Range("A3").Value = "1e3": s = s & Dmp(ws.Range("A3").Value2) & ws.Range("A3").NumberFormat & "|"
    ws.Range("A4").Value = "5%": s = s & Dmp(ws.Range("A4").Value2) & ws.Range("A4").NumberFormat & "|"
    ws.Range("A5").Value = "$5": s = s & Dmp(ws.Range("A5").Value2) & ws.Range("A5").NumberFormat & "|"
    ws.Range("A6").Value = "1,234": s = s & Dmp(ws.Range("A6").Value2) & ws.Range("A6").NumberFormat & "|"
    ws.Range("A7").Value = "(5)": s = s & Dmp(ws.Range("A7").Value2) & ws.Range("A7").NumberFormat & "|"
    ws.Range("A8").Value = "1/2": s = s & Dmp(ws.Range("A8").Value2) & ws.Range("A8").NumberFormat & "|"
    ws.Range("A9").Value = "'5": s = s & Dmp(ws.Range("A9").Value2) & ws.Range("A9").PrefixCharacter & "|"
    ws.Range("A10").Value = "=A1": s = s & Dmp(ws.Range("A10").Value2) & "|"
    ws.Range("A11").Value = "#N/A": s = s & Dmp(ws.Range("A11").Value2) & "|"
    ws.Range("A12").Value = "TRUE": s = s & Dmp(ws.Range("A12").Value2) & "|"
    ws.Range("A13").Value = "1 1/2": s = s & Dmp(ws.Range("A13").Value2) & ws.Range("A13").NumberFormat & "|"
    ws.Range("A14").Value = "Jan-00": s = s & Dmp(ws.Range("A14").Value2) & ws.Range("A14").NumberFormat & "|"
    ws.Range("A15").Value = "-": s = s & Dmp(ws.Range("A15").Value2) & "|"
    s = s & ErrS()
    R = s
Done:
    M74 = R
    Exit Function
EH:
    M74 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M75() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("B2:D4").Value = 1
    ws.Range("B2").Formula = "=SUM(C2:D4)"
    Set rg = ws.Range("B2:D4")
    On Error Resume Next
    s = rg.Offset(1, 1).Address
    s = s & rg.Offset(-1, -1).Address
    s = s & rg.Offset(, 1).Address
    s = s & rg.Offset(1).Address
    s = s & rg.Offset(0, 0).Address
    s = s & ErrS()
    s = s & rg.Offset(-2, 0).Address
    s = s & ErrS()
    s = s & rg.Offset(0, -2).Address
    s = s & ErrS()
    s = s & rg.Offset(1048576, 0).Address
    s = s & ErrS()
    s = s & rg.Offset(1048573, 0).Address
    s = s & ErrS()
    s = s & rg.Offset(0, 16381).Address
    s = s & ErrS()
    s = s & rg.Offset(0, 16382).Address
    s = s & ErrS()
    s = s & rg.Resize(1, 1).Address
    s = s & rg.Resize(5).Address
    s = s & rg.Resize(, 5).Address
    s = s & rg.Resize(0).Address
    s = s & ErrS()
    s = s & rg.Resize(1, 0).Address
    s = s & ErrS()
    s = s & rg.Resize(-1).Address
    s = s & ErrS()
    s = s & rg.Resize(1048577).Address
    s = s & ErrS()
    s = s & rg.Resize(1048576).Address
    s = s & ErrS()
    s = s & rg.Resize(1.5, 2.5).Address
    s = s & rg.Resize(2, 16385).Address
    s = s & ErrS()
    s = s & rg.Offset("1", "1").Address
    s = s & ErrS()
    s = s & rg.Offset("a").Address
    s = s & ErrS()
    s = s & rg.Offset(Null).Address
    s = s & ErrS()
    s = s & rg.Offset(True, True).Address
    s = s & ErrS()
    s = s & rg.Offset(Empty, 1).Address
    s = s & ErrS()
    R = s
Done:
    M75 = R
    Exit Function
EH:
    M75 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M76() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("B2:C3").Value = 1
    ws.Range("E5").Value = 1
    ws.Range("B2:B3").Cells(2).Value = ""
    ws.Range("H1").Value = 1
    On Error Resume Next
    s = ws.Range("B2").End(xlDown).Address
    s = s & ws.Range("B2").End(xlToRight).Address
    s = s & ws.Range("B2").End(xlUp).Address
    s = s & ws.Range("B2").End(xlToLeft).Address
    s = s & ws.Range("B3").End(xlDown).Address
    s = s & ws.Range("C3").End(xlDown).Address
    s = s & ws.Range("C3").End(xlToRight).Address
    s = s & ws.Range("E5").End(xlToLeft).Address
    s = s & ws.Range("E5").End(xlUp).Address
    s = s & ws.Range("A1").End(xlToRight).Address
    s = s & ws.Range("A1").End(xlDown).Address
    s = s & ws.Range("B2:C3").End(xlDown).Address
    s = s & ws.Range("B2:C3").End(xlToRight).Address
    s = s & ws.Range("B2,E5").End(xlDown).Address
    s = s & ErrS()
    s = s & ws.Range("B2").End(0).Address
    s = s & ErrS()
    s = s & ws.Range("B2").End(5).Address
    s = s & ErrS()
    s = s & ws.Range("B2").End("a").Address
    s = s & ErrS()
    s = s & ws.Range("B2").End(-4121).Address
    s = s & ws.Range("B2").End(-4161).Address
    R = s
Done:
    M76 = R
    Exit Function
EH:
    M76 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M77() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("B2:C3").Value = 1
    ws.Range("D4").Value = 1
    ws.Range("F2").Value = 1
    s = ws.Range("B2").CurrentRegion.Address & ws.Range("C3").CurrentRegion.Address & ws.Range("D4").CurrentRegion.Address & ws.Range("A1").CurrentRegion.Address & ws.Range("E3").CurrentRegion.Address & ws.Range("B2:B3").CurrentRegion.Address & ws.Range("B2,F2").CurrentRegion.Address & ws.Range("F2").CurrentRegion.Address & ws.Range("D3").CurrentRegion.Address & ws.Cells.CurrentRegion.Address & ws.Range("A:A").CurrentRegion.Address
    s = s & ws.Range("B2").CurrentArray.Address
    ws.Range("H1:H3").FormulaArray = "=ROW(1:3)"
    s = s & ws.Range("H2").CurrentArray.Address & ws.Range("H2").HasArray & ws.Range("H1").HasArray & ws.Range("B2").HasArray
    R = s
Done:
    M77 = R
    Exit Function
EH:
    M77 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M78() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:A6").Value = Application.Transpose(Array(1, "a", "", 2, "=1", ""))
    ws.Range("B1").Formula = "=1/0": ws.Range("B2").Formula = "=""x""": ws.Range("B3").Formula = "=TRUE": ws.Range("B4").Formula = "=1": ws.Range("B5").Value = True
    On Error Resume Next
    Dim t As Variant
    For Each t In Array(xlCellTypeBlanks, xlCellTypeConstants, xlCellTypeFormulas, xlCellTypeLastCell, xlCellTypeVisible, xlCellTypeAllFormatConditions, xlCellTypeAllValidation, xlCellTypeComments, xlCellTypeSameFormatConditions, xlCellTypeSameValidation)
        Set rg = Nothing
        Set rg = ws.Range("A1:B6").SpecialCells(t)
        s = s & t
        s = s & "="
        s = s & rg.Address(0, 0)
        s = s & ErrS()
        s = s & ";"
    Next t
    Dim u As Variant
    For Each u In Array(1, 2, 4, 16, 23, 3, 7)
        Set rg = Nothing
        Set rg = ws.Range("A1:B6").SpecialCells(xlCellTypeConstants, u)
        s = s & "c"
        s = s & u
        s = s & "="
        s = s & rg.Address(0, 0)
        s = s & ErrS()
        s = s & ";"
        Set rg = Nothing
        Set rg = ws.Range("A1:B6").SpecialCells(xlCellTypeFormulas, u)
        s = s & "f"
        s = s & u
        s = s & "="
        s = s & rg.Address(0, 0)
        s = s & ErrS()
        s = s & ";"
    Next u
    Set rg = ws.Range("A1").SpecialCells(xlCellTypeConstants): s = s & rg.Address & ErrS()
    Set rg = ws.Range("A2").SpecialCells(xlCellTypeBlanks): s = s & rg.Address & ErrS()
    Set rg = ws.Cells.SpecialCells(xlCellTypeLastCell): s = s & rg.Address & ErrS()
    Set rg = ws.Range("A1:B6").SpecialCells(99): s = s & ErrS()
    R = s
Done:
    M78 = R
    Exit Function
EH:
    M78 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M79() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range, f As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1").Value = "Apple": ws.Range("A2").Value = "apple pie": ws.Range("A3").Value = 12: ws.Range("A4").Formula = "=A3*2": ws.Range("A5").Value = "APPLE"
    ws.Range("B1").Value = "a*b": ws.Range("B2").Value = "a?b": ws.Range("B3").Value = "~": ws.Range("B4").Value = 1.5: ws.Range("B5").Value = CDate("1/2/2000")
    On Error Resume Next
    Set rg = ws.Range("A1:B5")
    Dim w As Variant
    For Each w In Array("apple", "app*", "a?ple", "*", "?", "~*", "a~*b", "a*b", "12", 12, 24, "1.5", 1.5, "2000", "1/2/2000", "~~", "", "pie")
        Set f = Nothing
        Set f = rg.Find(w)
        s = s & Dmp(w)
        s = s & IIf(f Is Nothing, "-", f.Address(0, 0))
        s = s & ErrS()
        Set f = Nothing
        Set f = rg.Find(w, , xlValues, xlWhole)
        s = s & IIf(f Is Nothing, "-", f.Address(0, 0))
        Set f = Nothing
        Set f = rg.Find(w, , xlFormulas, xlPart, , , True)
        s = s & IIf(f Is Nothing, "-", f.Address(0, 0))
        Set f = Nothing
        Set f = rg.Find(w, rg.Cells(1, 1), xlValues, xlPart, xlByColumns, xlPrevious)
        s = s & IIf(f Is Nothing, "-", f.Address(0, 0))
        s = s & ";"
    Next w
    R = s
Done:
    M79 = R
    Exit Function
EH:
    M79 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M80() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range, f As Range, g As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:A6").Value = Application.Transpose(Array("x1", "y", "x2", "y", "x3", "z"))
    Set rg = ws.Range("A1:A6")
    On Error Resume Next
    Set f = rg.Find("x")
    s = f.Address(0, 0)
    Set g = rg.FindNext(f): s = s & g.Address(0, 0)
    Set g = rg.FindNext(g): s = s & g.Address(0, 0)
    Set g = rg.FindNext(g): s = s & g.Address(0, 0)
    Set g = rg.FindPrevious(g): s = s & g.Address(0, 0)
    Set g = rg.FindNext: s = s & g.Address(0, 0) & ErrS()
    Set g = rg.FindNext(rg.Cells(6)): s = s & g.Address(0, 0) & ErrS()
    Set g = rg.FindNext(ws.Range("C1")): s = s & g.Address(0, 0) & ErrS()
    Set g = rg.FindNext(Worksheets("Sheet2").Range("A1")): s = s & g.Address(0, 0) & ErrS()
    Set g = rg.FindNext(rg): s = s & g.Address(0, 0) & ErrS()
    Set f = rg.Find("y", , , , , , , , True): s = s & f.Address(0, 0) & ErrS()
    Set f = rg.Find("q"): s = s & TypeName(f) & ErrS()
    Set g = rg.FindNext(): s = s & TypeName(g) & ErrS()
    Set f = rg.Find("x", after:=rg.Cells(1), SearchDirection:=xlPrevious): s = s & f.Address(0, 0)
    Set f = rg.Find(What:="x", LookIn:=99): s = s & ErrS()
    Set f = rg.Find(What:="x", LookAt:=99): s = s & ErrS()
    Set f = rg.Find(What:="x", SearchOrder:=99): s = s & ErrS()
    Set f = rg.Find(What:="x", SearchDirection:=99): s = s & ErrS()
    Set f = rg.Find(Null): s = s & ErrS()
    Set f = rg.Find(Empty): s = s & TypeName(f) & ErrS()
    R = s
Done:
    M80 = R
    Exit Function
EH:
    M80 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M81() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, nm As Name, wb As Workbook
    Set ws = Worksheets("Sheet1")
    Set wb = ThisWorkbook
    ws.Cells.Clear
    On Error Resume Next
    wb.Names.Add "nA", "=Sheet1!$A$1:$B$2"
    wb.Names.Add "nB", "=A1+1"
    wb.Names.Add "nC", "5"
    wb.Names.Add "nD", "=""x"""
    wb.Names.Add "nE", "=Sheet1!R1C1", , , True
    wb.Names.Add "nF", "=Sheet1!R1C1"
    s = s & ErrS()
    For Each nm In wb.Names
        s = s & nm.Name
        s = s & "|"
        s = s & nm.RefersTo
        s = s & "|"
        s = s & nm.RefersToLocal
        s = s & "|"
        s = s & nm.RefersToR1C1
        s = s & "|"
        s = s & nm.RefersToR1C1Local
        s = s & "|"
        s = s & nm.Visible
        s = s & nm.Index
        s = s & nm.Value
        s = s & ErrS()
        s = s & nm.NameLocal
        s = s & "|"
        s = s & nm.Comment
        s = s & "|"
        s = s & nm.MacroType
        s = s & nm.ValidWorkbookParameter
        s = s & ";"
    Next nm
    s = s & wb.Names("nA").RefersToRange.Address
    s = s & ErrS()
    s = s & wb.Names("nC").RefersToRange.Address
    s = s & ErrS()
    s = s & wb.Names("nB").RefersToRange.Address
    s = s & ErrS()
    wb.Names("nC").RefersTo = "=1+": s = s & ErrS()
    wb.Names("nC").RefersToLocal = "=1;2": s = s & ErrS()
    wb.Names("nC").RefersToR1C1 = "=R[1]C[1]": s = s & wb.Names("nC").RefersTo & ErrS()
    wb.Names("nC").Name = "nC2": s = s & wb.Names("nC2").Name & ErrS()
    wb.Names("nC2").Name = "1bad": s = s & ErrS()
    wb.Names("nC2").Name = "R1C1": s = s & ErrS()
    wb.Names("nC2").Name = "a b": s = s & ErrS()
    wb.Names("nC2").Name = "A1": s = s & ErrS()
    wb.Names("nC2").Name = "_ok.name\x?": s = s & wb.Names(wb.Names.Count).Name & ErrS()
    wb.Names("nD").Visible = False: s = s & wb.Names.Count & ErrS()
    wb.Names("zz").Delete: s = s & ErrS()
    wb.Names.Item(0): s = s & ErrS()
    wb.Names.Item(99): s = s & ErrS()
    wb.Names.Add "a" & String(260, "b"), "=1": s = s & ErrS()
    wb.Names.Add "", "=1": s = s & ErrS()
    wb.Names.Add , "=1": s = s & ErrS()
    wb.Names.Add "ok1": s = s & ErrS()
    wb.Names("ok1").RefersTo = "=Sheet1!A1": s = s & wb.Names("ok1").RefersTo & ErrS()
    ws.Names.Add "loc", "=Sheet1!$C$3": s = s & ws.Names("loc").Name & ws.Names.Count & ErrS()
    ws.Names.Add "Sheet2!loc2", "=Sheet1!$C$3": s = s & ErrS()
    R = s
Done:
    M81 = R
    Exit Function
EH:
    M81 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M82() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("B2").Formula = "=SUM(A1:A3)+Sheet2!C4"
    ws.Range("B3").FormulaR1C1 = "=R[-1]C+RC[-1]+R1C1+R[0]C[0]"
    ws.Range("B4").Formula2 = "=SEQUENCE(2)"
    ws.Range("B7").Formula = "=SEQUENCE(2)"
    ws.Range("B10").FormulaArray = "=SUM(A1:A3*2)"
    ws.Range("B11").Formula = "=@A1:A3"
    ws.Range("B12").Formula2 = "=@A1:A3"
    On Error Resume Next
    Dim i As Long
    Dim a As Variant
    For Each a In Array(2, 3, 4, 5, 7, 8, 10, 11, 12)
        s = s & Dmp(ws.Cells(a, 2).Formula)
        s = s & Dmp(ws.Cells(a, 2).Formula2)
        s = s & Dmp(ws.Cells(a, 2).FormulaR1C1)
        s = s & Dmp(ws.Cells(a, 2).Formula2R1C1)
        s = s & Dmp(ws.Cells(a, 2).FormulaLocal)
        s = s & Dmp(ws.Cells(a, 2).HasFormula)
        s = s & Dmp(ws.Cells(a, 2).HasSpill)
        s = s & Dmp(ws.Cells(a, 2).HasArray)
        s = s & ErrS()
        s = s & ";"
    Next a
    R = s
Done:
    M82 = R
    Exit Function
EH:
    M82 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M83() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    On Error Resume Next
    ws.Range("A1").Formula = "=1+": s = s & ErrS()
    ws.Range("A1").Formula = "=SUM(": s = s & ErrS()
    ws.Range("A1").Formula = "=NOSUCH(1)": s = s & ErrS() & ws.Range("A1").Formula & Dmp(ws.Range("A1").Value2) & "|"
    ws.Range("A1").Formula = "=A1": s = s & ErrS() & Dmp(ws.Range("A1").Value2) & "|"
    ws.Range("A1").Formula = "=""a""&""b""": s = s & Dmp(ws.Range("A1").Value2)
    ws.Range("A1").Formula = "= 1 + 2": s = s & ws.Range("A1").Formula & "|"
    ws.Range("A1").Formula = "=sum(1,2)": s = s & ws.Range("A1").Formula & "|"
    ws.Range("A1").Formula = "=1;2": s = s & ErrS()
    ws.Range("A1").Formula = "=1,2": s = s & ErrS()
    ws.Range("A1").Formula = "=[1]A1": s = s & ErrS()
    ws.Range("A1").Formula = "=Sheet9!A1": s = s & ErrS() & ws.Range("A1").Formula & "|"
    ws.Range("A1").Formula = "=#REF!": s = s & ErrS() & Dmp(ws.Range("A1").Value2)
    ws.Range("A1").Formula = "=nm_undefined": s = s & ErrS() & Dmp(ws.Range("A1").Value2)
    ws.Range("A1").Formula = "'=1+1": s = s & ErrS() & Dmp(ws.Range("A1").Value2)
    ws.Range("A1").Formula = "==1": s = s & ErrS()
    ws.Range("A1").Formula = "=" & String(8200, "1"): s = s & ErrS()
    ws.Range("A1").Value = String(32768, "a"): s = s & ErrS()
    ws.Range("A1").Value = String(32767, "a"): s = s & ErrS() & Len(ws.Range("A1").Value) & Len(ws.Range("A1").Text)
    R = s
Done:
    M83 = R
    Exit Function
EH:
    M83 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M84() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1").Value = 1234.5
    On Error Resume Next
    Dim f As Variant
    For Each f In Array("0.00", "#,##0", "yyyy-mm-dd", "General", "@", "0.0%", "[Red]0", "[$-409]d mmmm", "0;0;0;@", "# ?/?", "h:mm", "dd/mm/yy", "[>100]0;0", "0.00E+00", """x""0", "\x0", "0*-", "_(* #,##0_)", "[h]:mm:ss", "mm:ss.0", "m/d/yyyy", "", "Standard", "bad""", "0.0.0", "[Blue][Red]0", "General General", "#,##0.00_);(#,##0.00)", "[DBNum1]0", "[$$-409]#,##0.00", "yyyy""" & ChrW(24180) & """", "ddd", "aaaa", "gge")
        ws.Range("A1").NumberFormat = f
        s = s & ErrS()
        s = s & "["
        s = s & ws.Range("A1").NumberFormat
        s = s & "|"
        s = s & ws.Range("A1").NumberFormatLocal
        s = s & "|"
        s = s & ws.Range("A1").Text
        s = s & "]"
    Next f
    ws.Range("A1").NumberFormatLocal = "0.000": s = s & ws.Range("A1").NumberFormat & ErrS()
    ws.Range("A1").NumberFormatLocal = "dd/mm/yyyy": s = s & ws.Range("A1").NumberFormat & ws.Range("A1").Text & ErrS()
    ws.Range("A1").NumberFormat = Null: s = s & ErrS()
    ws.Range("A1").NumberFormat = 5: s = s & ErrS() & ws.Range("A1").NumberFormat
    R = s
Done:
    M84 = R
    Exit Function
EH:
    M84 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M85() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1").Value = "Hello World"
    On Error Resume Next
    s = ws.Range("A1").Characters.Text
    s = s & "|"
    s = s & ws.Range("A1").Characters(1, 5).Text
    s = s & "|"
    s = s & ws.Range("A1").Characters(7).Text
    s = s & "|"
    s = s & ws.Range("A1").Characters(7, 100).Text
    s = s & "|"
    s = s & ws.Range("A1").Characters(0, 3).Text
    s = s & ErrS()
    s = s & "|"
    s = s & ws.Range("A1").Characters(20, 3).Text
    s = s & ErrS()
    s = s & "|"
    s = s & ws.Range("A1").Characters(1, 0).Text
    s = s & ErrS()
    s = s & "|"
    s = s & ws.Range("A1").Characters(-1, 3).Text
    s = s & ErrS()
    s = s & "|"
    s = s & ws.Range("A1").Characters(1, -1).Text
    s = s & ErrS()
    s = s & ws.Range("A1").Characters(1, 5).Count
    s = s & ws.Range("A1").Characters.Count
    s = s & ErrS()
    ws.Range("A1").Characters(1, 5).Font.Bold = True: s = s & ErrS() & ws.Range("A1").Characters(1, 1).Font.Bold & ws.Range("A1").Characters(7, 1).Font.Bold & Dmp(ws.Range("A1").Font.Bold)
    ws.Range("A1").Characters(1, 5).Text = "Bye": s = s & ErrS() & ws.Range("A1").Value
    ws.Range("A1").Characters(1, 5).Delete: s = s & ErrS() & ws.Range("A1").Value
    ws.Range("A1").Characters(1, 1).Insert "ZZ": s = s & ErrS() & ws.Range("A1").Value
    ws.Range("A2").Formula = "=1+1"
    s = s & ws.Range("A2").Characters(1, 1).Text
    s = s & ErrS()
    ws.Range("A3").Value = 123456
    s = s & ws.Range("A3").Characters(2, 2).Text
    s = s & ErrS()
    R = s
Done:
    M85 = R
    Exit Function
EH:
    M85 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M86() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    On Error Resume Next
    Dim c As Comment
    ws.Range("A1").AddComment "first": s = s & ErrS() & ws.Range("A1").Comment.Text & "|" & ws.Comments.Count
    ws.Range("A1").AddComment "second": s = s & ErrS()
    ws.Range("A2").AddComment: s = s & ErrS() & Dmp(ws.Range("A2").Comment.Text)
    ws.Range("A3:A4").AddComment "multi": s = s & ErrS()
    ws.Range("A5").AddComment Null: s = s & ErrS()
    ws.Range("A6").AddComment 123: s = s & ErrS() & ws.Range("A6").Comment.Text
    ws.Range("A1").Comment.Text "appended", 1, False: s = s & ErrS() & ws.Range("A1").Comment.Text
    ws.Range("A1").Comment.Text Text:="replaced": s = s & ErrS() & ws.Range("A1").Comment.Text
    ws.Range("A1").Comment.Text "Z", 3, True: s = s & ErrS() & ws.Range("A1").Comment.Text
    ws.Range("A1").Comment.Text "Q", 100, False: s = s & ErrS() & ws.Range("A1").Comment.Text
    ws.Range("A1").Comment.Text "Q", 0, False: s = s & ErrS() & ws.Range("A1").Comment.Text
    s = s & ws.Range("A1").Comment.Author
    s = s & "|"
    s = s & ws.Range("A1").Comment.Visible
    s = s & ws.Range("A1").Comment.Parent.Address
    s = s & ErrS()
    s = s & ws.Range("B9").Comment.Text
    s = s & ErrS()
    s = s & ws.Comments(1).Parent.Address
    s = s & ws.Comments(99).Text
    s = s & ErrS()
    s = s & ws.Comments.Count
    ws.Range("A1").Comment.Delete: s = s & ws.Comments.Count
    ws.Range("A1").ClearComments: ws.Range("A2:A6").ClearComments: s = s & ws.Comments.Count & ErrS()
    ws.Range("A1").Comment.Delete: s = s & ErrS()
    ws.Range("A1").AddCommentThreaded "t": s = s & ErrS() & ws.Range("A1").CommentThreaded.Text & ws.CommentsThreaded.Count & ws.Comments.Count
    ws.Range("A1").CommentThreaded.AddReply "r": s = s & ErrS() & ws.Range("A1").CommentThreaded.Replies.Count
    ws.Range("A1").AddComment "n": s = s & ErrS()
    ws.Range("A1").CommentThreaded.Delete: s = s & ErrS() & ws.CommentsThreaded.Count
    R = s
Done:
    M86 = R
    Exit Function
EH:
    M86 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M87() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, wb As Workbook, ws As Worksheet
    Set wb = ThisWorkbook
    On Error Resume Next
    s = wb.Worksheets(0).Name
    s = s & ErrS()
    s = s & wb.Worksheets(99).Name
    s = s & ErrS()
    s = s & wb.Worksheets("nope").Name
    s = s & ErrS()
    s = s & wb.Worksheets("sheet1").Name
    s = s & ErrS()
    s = s & wb.Worksheets(1.5).Name
    s = s & ErrS()
    s = s & wb.Worksheets(Null).Name
    s = s & ErrS()
    s = s & wb.Worksheets(True).Name
    s = s & ErrS()
    s = s & wb.Worksheets(Empty).Name
    s = s & ErrS()
    s = s & wb.Sheets(0).Name
    s = s & ErrS()
    s = s & wb.Charts(1).Name
    s = s & ErrS()
    s = s & wb.Worksheets("").Name
    s = s & ErrS()
    s = s & wb.Worksheets(Array(1, 2)).Count
    s = s & ErrS()
    s = s & wb.Worksheets(Array("Sheet1", "Nope")).Count
    s = s & ErrS()
    s = s & wb.Sheets(Array(2, 1)).Count
    s = s & ErrS()
    s = s & wb.Worksheets.Item(1).Index
    s = s & ErrS()
    s = s & wb.Worksheets(-1).Name
    s = s & ErrS()
    R = s
Done:
    M87 = R
    Exit Function
EH:
    M87 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M88() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, wb As Workbook, ws As Worksheet
    Set wb = ThisWorkbook
    On Error Resume Next
    s = Application.Workbooks(0).Name
    s = s & ErrS()
    s = s & Application.Workbooks(99).Name
    s = s & ErrS()
    s = s & Application.Workbooks("nope.xlsx").Name
    s = s & ErrS()
    s = s & Application.Workbooks("").Name
    s = s & ErrS()
    s = s & Application.Workbooks.Open("C:\nonexistent\x.xlsx").Name
    s = s & ErrS()
    s = s & Application.Workbooks.Open("").Name
    s = s & ErrS()
    s = s & Application.Workbooks.Open("C:\nonexistent\").Name
    s = s & ErrS()
    s = s & Application.Workbooks.Open("a:b*.xlsx").Name
    s = s & ErrS()
    R = s
Done:
    M88 = R
    Exit Function
EH:
    M88 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M89() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    On Error Resume Next
    ws.Name = "Sheet2": s = s & ErrS()
    ws.Name = "sheet2": s = s & ErrS()
    ws.Name = "": s = s & ErrS()
    ws.Name = "a:b": s = s & ErrS()
    ws.Name = "a/b": s = s & ErrS()
    ws.Name = "a\b": s = s & ErrS()
    ws.Name = "a?b": s = s & ErrS()
    ws.Name = "a*b": s = s & ErrS()
    ws.Name = "a[b": s = s & ErrS()
    ws.Name = "a]b": s = s & ErrS()
    ws.Name = "'a": s = s & ErrS()
    ws.Name = "a'": s = s & ErrS()
    ws.Name = "a'b": s = s & ErrS() & ws.Name
    ws.Name = "History": s = s & ErrS()
    ws.Name = "history": s = s & ErrS()
    ws.Name = String(31, "x"): s = s & ErrS()
    ws.Name = String(32, "x"): s = s & ErrS()
    ws.Name = " ": s = s & ErrS()
    ws.Name = "a b": s = s & ErrS()
    ws.Name = 123: s = s & ErrS() & ws.Name
    ws.Name = Null: s = s & ErrS()
    ws.Name = True: s = s & ErrS() & ws.Name
    ws.Name = "Sheet1": s = s & ErrS() & ws.Name
    R = s
Done:
    M89 = R
    Exit Function
EH:
    M89 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M90() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Dmp(Application.Evaluate("1+1"))
    s = s & Dmp(Application.Evaluate("1/0"))
    s = s & Dmp(Application.Evaluate("A1:A2"))
    s = s & Dmp(Application.Evaluate("{1,2;3,4}"))
    s = s & Dmp(Application.Evaluate("""a""&""b"""))
    s = s & Dmp(Application.Evaluate("SUM("))
    s = s & ErrS()
    s = s & Dmp(Application.Evaluate(""))
    s = s & ErrS()
    s = s & Dmp(Application.Evaluate("NoName"))
    s = s & Dmp(Application.Evaluate("Sheet1!A1"))
    s = s & Dmp(Application.Evaluate("[Sheet1]A1"))
    s = s & Dmp(Application.Evaluate("ROW(A1:A3)"))
    s = s & Dmp(Application.Evaluate("SEQUENCE(2)"))
    s = s & Dmp(Application.Evaluate("TODAY()>0"))
    s = s & Dmp(Application.Evaluate("A1"))
    s = s & Dmp(Application.Evaluate(1))
    s = s & ErrS()
    s = s & Dmp(Application.Evaluate(Null))
    s = s & ErrS()
    s = s & Dmp(Application.Evaluate("1,2"))
    s = s & Dmp(Application.Evaluate("=1+1"))
    s = s & Dmp(Application.Evaluate("'x'"))
    s = s & Dmp(Application.Evaluate("#N/A"))
    s = s & Dmp(Application.Evaluate("TRUE"))
    s = s & Dmp(Application.Evaluate("1E5"))
    s = s & Dmp(Application.Evaluate(Space(5)))
    R = s
Done:
    M90 = R
    Exit Function
EH:
    M90 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M91() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Dmp(Application.WorksheetFunction.Sum(1, 2, "3"))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Average(Array()))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.VLookup(1, Array(1, 2), 2, False))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Match("x", Array("a"), 0))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Log(0))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Sqrt(-1))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Pi)
    s = s & Dmp(Application.WorksheetFunction.Round(2.5, 0))
    s = s & Dmp(Application.WorksheetFunction.Text(1.5, "0.00"))
    s = s & Dmp(Application.WorksheetFunction.Concat("a", 1))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Roman(4))
    s = s & Dmp(Application.WorksheetFunction.Roman(0))
    s = s & Dmp(Application.WorksheetFunction.Roman(4000))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Index(Array(1, 2), 3))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Large(Array(1, 2), 3))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Sum(Null))
    s = s & ErrS()
    s = s & Dmp(Application.Sum(1, 2))
    s = s & Dmp(Application.VLookup(1, Array(1, 2), 2, False))
    s = s & Dmp(Application.Log(0))
    s = s & Dmp(Application.Sqrt(-1))
    s = s & Dmp(Application.Match("x", Array("a"), 0))
    s = s & Dmp(Application.Index(Array(1, 2), 3))
    s = s & Dmp(Application.Large(Array(1, 2), 3))
    s = s & Dmp(Application.Text(1.5, "0.00"))
    s = s & Dmp(Application.Max())
    s = s & ErrS()
    s = s & Dmp(Application.Transpose(Array(1, 2)))
    s = s & Dmp(Application.Transpose(5))
    s = s & ErrS()
    s = s & Dmp(Application.Round(1.5))
    s = s & ErrS()
    s = s & Dmp(Application.Proper("ab cd"))
    s = s & Dmp(Application.IsNumber("5"))
    s = s & Dmp(Application.CountIf(Array(1, 2, 2), 2))
    s = s & ErrS()
    R = s
Done:
    M91 = R
    Exit Function
EH:
    M91 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M92() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:A3").Value = Application.Transpose(Array(1, 2, 3))
    On Error Resume Next
    s = Dmp(ws.Range("A1:A3").Value)
    s = s & Dmp(Application.Sum(ws.Range("A1:A3")))
    s = s & Dmp(Application.Sum(ws.Range("A1:A3").Value))
    s = s & Dmp(Application.WorksheetFunction.SumProduct(ws.Range("A1:A3"), ws.Range("A1:A3")))
    s = s & Dmp(Application.WorksheetFunction.Transpose(ws.Range("A1:A3")))
    s = s & Dmp(Application.WorksheetFunction.MMult(ws.Range("A1:A3"), ws.Range("A1:A3")))
    s = s & ErrS()
    s = s & TypeName(ws.Range("A1:A3").Value)
    s = s & TypeName(ws.Range("A1").Value)
    s = s & UBound(ws.Range("A1:A3").Value)
    s = s & UBound(ws.Range("A1:A3").Value, 2)
    s = s & LBound(ws.Range("A1:A3").Value)
    s = s & TypeName(ws.Range("A1:C1").Value2)
    Dim v As Variant
    v = ws.Range("A1:A3").Value
    v(1, 1) = 99: s = s & ws.Range("A1").Value & v(1, 1) & ErrS()
    ws.Range("B1:B3").Value = v: s = s & ws.Range("B1").Value & ErrS()
    ws.Range("B1:B3").Value = Array(7, 8, 9): s = s & ws.Range("B1").Value & ws.Range("B2").Value & ws.Range("B3").Value & ErrS()
    ws.Range("C1:E1").Value = Array(7, 8, 9): s = s & ws.Range("C1").Value & ws.Range("D1").Value & ws.Range("E1").Value & ErrS()
    ws.Range("C2:E3").Value = Array(7, 8, 9): s = s & ws.Range("C2").Value & ws.Range("D2").Value & ws.Range("C3").Value & ErrS()
    ws.Range("F1:G2").Value = Array(1, 2, 3): s = s & Dmp(ws.Range("F1:G2").Value) & ErrS()
    ws.Range("H1:H2").Value = Array(1): s = s & Dmp(ws.Range("H1:H2").Value) & ErrS()
    ws.Range("A1:A3").Value = ws.Range("B1:B2").Value: s = s & Dmp(ws.Range("A1:A3").Value) & ErrS()
    ws.Range("A1:A3").Value = ws.Range("C1:E1").Value: s = s & Dmp(ws.Range("A1:A3").Value) & ErrS()
    ws.Range("A1:A3").Value = ws.Range("A1"): s = s & Dmp(ws.Range("A1:A3").Value) & ErrS()
    R = s
Done:
    M92 = R
    Exit Function
EH:
    M92 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M93() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    On Error Resume Next
    s = ws.Range("A1").Address & ws.Range("A1").Address(False) & ws.Range("A1").Address(False, False) & ws.Range("A1").Address(, False) & ws.Range("A1").Address(xlA1, xlR1C1) & ErrS() & ws.Range("A1:B2").Address(ReferenceStyle:=xlR1C1) & ws.Range("A1:B2").Address(False, False, xlR1C1) & ws.Range("B2").Address(True, True, xlR1C1, , ws.Range("A1")) & ws.Range("B2").Address(False, False, xlR1C1, , ws.Range("D5"))
    s = s & ws.Range("B2").Address(False, False, xlA1, , ws.Range("D5"))
    s = s & ""
    s = s & ws.Range("A1,C3").Address
    s = s & ws.Range("A1,C3").Address(False, False, xlR1C1)
    s = s & ws.Range("A:A").Address
    s = s & ws.Range("2:2").Address
    s = s & ws.Range("A:B").Address(False, False)
    s = s & ws.Range("1:3").Address(False, False, xlR1C1)
    s = s & ws.Cells.Address
    s = s & ws.Range("A1").Address(99)
    s = s & ErrS()
    s = s & ws.Range("A1").Address("x")
    s = s & ErrS()
    s = s & ws.Range("A1").Address(Null)
    s = s & ErrS()
    s = s & ws.Range("A1").Address(xlA1, 5)
    s = s & ErrS()
    s = s & ws.Range("A1:B2").AddressLocal
    s = s & ws.Range("A1:B2").AddressLocal(False, False)
    R = s
Done:
    M93 = R
    Exit Function
EH:
    M93 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M94() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    On Error Resume Next
    s = ws.Range("A1:B2").Cells(2, 2).Address
    s = s & ws.Range("A1:B2").Cells(3, 3).Address
    s = s & ws.Range("A1:B2")(2, 2).Address
    s = s & ws.Range("A1:B2")(5).Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2").Cells(0, 0).Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2").Cells(1, 0).Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2").Cells(-1, 1).Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2").Item(2).Address
    s = s & ws.Range("A1:B2").Item(2, 2).Address
    s = s & ws.Range("A1:B2").Item("A").Address
    s = s & ErrS()
    s = s & ws.Cells(1048576, 16384).Address
    s = s & ws.Cells(1048577, 1).Address
    s = s & ErrS()
    s = s & ws.Cells(0, 1).Address
    s = s & ErrS()
    s = s & ws.Cells("A", 1).Address
    s = s & ErrS()
    s = s & ws.Cells(1, "B").Address
    s = s & ErrS()
    s = s & ws.Cells(1, "AA").Address
    s = s & ErrS()
    s = s & ws.Cells(1.5, 1.5).Address
    s = s & ws.Cells(2.5, 2.5).Address
    s = s & ws.Cells(Null, 1).Address
    s = s & ErrS()
    s = s & ws.Cells(, 1).Address
    s = s & ErrS()
    s = s & ws.Cells(1).Address
    s = s & ws.Cells(16385).Address
    s = s & ws.Cells(17179869184#).Address
    s = s & ErrS()
    s = s & ws.Cells(17179869185#).Address
    s = s & ErrS()
    s = s & ws.Cells.Item(2, 3).Address
    s = s & ws.Rows(2).Address
    s = s & ws.Rows("2:3").Address
    s = s & ws.Rows("a").Address
    s = s & ErrS()
    s = s & ws.Columns("B").Address
    s = s & ws.Columns("B:C").Address
    s = s & ws.Columns(3).Address
    s = s & ws.Columns("1").Address
    s = s & ErrS()
    s = s & ws.Rows(0).Address
    s = s & ErrS()
    s = s & ws.Columns(16385).Address
    s = s & ErrS()
    s = s & ws.Rows.Count
    s = s & ws.Columns.Count
    R = s
Done:
    M94 = R
    Exit Function
EH:
    M94 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M95() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    On Error Resume Next
    s = ws.Range("A1").Address
    s = s & ws.Range("A1:B2").Address
    s = s & ws.Range("A1", "B2").Address
    s = s & ws.Range("B2", "A1").Address
    s = s & ws.Range("A1", "A1").Address
    s = s & ws.Range("A1:B2", "D4").Address
    s = s & ws.Range("A1:B2", "C3:D4").Address
    s = s & ws.Range("D4", ws.Range("A1:B2")).Address
    s = s & ws.Range("A1,C3", "E5").Address
    s = s & ErrS()
    s = s & ws.Range("a1").Address
    s = s & ws.Range("$A$1").Address
    s = s & ws.Range("A:A").Address
    s = s & ws.Range("A1:A").Address
    s = s & ErrS()
    s = s & ws.Range("1:1").Address
    s = s & ws.Range("a1:b2 b2:c3").Address
    s = s & ws.Range("A1:B2 C3").Address
    s = s & ErrS()
    s = s & ws.Range("A1,A1").Address
    s = s & ws.Range("A1,A1").Areas.Count
    s = s & ws.Range("A1:B2,B2:C3").Address
    s = s & ws.Range("A1:B2,B2:C3").Areas.Count
    s = s & ws.Range("A1:B2,B2:C3").Count
    s = s & ws.Range("B2:C3 A1:B2").Address
    s = s & ws.Range("R1C1").Address
    s = s & ErrS()
    s = s & ws.Range("A1:XFD1048576").Address
    s = s & ws.Range("XFE1").Address
    s = s & ErrS()
    s = s & ws.Range("A0").Address
    s = s & ErrS()
    s = s & ws.Range("").Address
    s = s & ErrS()
    s = s & ws.Range(" A1 ").Address
    s = s & ErrS()
    s = s & ws.Range("A1:B2,").Address
    s = s & ErrS()
    s = s & ws.Range(Null).Address
    s = s & ErrS()
    s = s & ws.Range(1).Address
    s = s & ErrS()
    s = s & ws.Range("Sheet1!A1").Address
    s = s & ErrS()
    s = s & ws.Range("Sheet2!A1").Address
    s = s & ErrS()
    s = s & ws.Range("[Book1]Sheet1!A1").Address
    s = s & ErrS()
    s = s & ws.Range("A1", Worksheets("Sheet2").Range("A2")).Address
    s = s & ErrS()
    s = s & ws.Range(ws.Cells(1, 1), ws.Cells(2, 2)).Address
    s = s & ws.Range(Worksheets("Sheet2").Cells(1, 1), ws.Cells(2, 2)).Address
    s = s & ErrS()
    R = s
Done:
    M95 = R
    Exit Function
EH:
    M95 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M96() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:C3").Value = 1
    Set rg = ws.Range("A1:C3")
    On Error Resume Next
    s = Application.Union(rg.Cells(1), rg.Cells(9)).Address
    s = s & Application.Union(ws.Range("A1"), ws.Range("A1")).Address
    s = s & Application.Union(ws.Range("A1:B2"), ws.Range("B2:C3")).Address
    s = s & Application.Union(ws.Range("A1"), Worksheets("Sheet2").Range("A1")).Address
    s = s & ErrS()
    s = s & Application.Intersect(ws.Range("A1:B2"), ws.Range("B2:C3")).Address
    s = s & TypeName(Application.Intersect(ws.Range("A1"), ws.Range("B2")))
    s = s & Application.Intersect(ws.Range("A1:B2"), ws.Range("B2:C3"), ws.Range("B2:B9")).Address
    s = s & Application.Intersect(ws.Range("A1,C3"), ws.Range("A1:B2")).Address
    s = s & ErrS()
    s = s & TypeName(Application.Intersect(ws.Range("A1"), Worksheets("Sheet2").Range("A1")))
    s = s & ErrS()
    s = s & Application.Intersect(ws.Range("A:A"), ws.Range("1:1")).Address
    s = s & Application.Union(ws.Range("A:A"), ws.Range("1:1")).Address
    s = s & Application.Union(ws.Range("A1:A2"), ws.Range("B1:B2")).Areas.Count
    s = s & rg.Rows.Address
    s = s & rg.Rows(2).Address
    s = s & rg.Rows("2:3").Address
    s = s & rg.Columns(2).Address
    s = s & rg.Columns("B:C").Address
    s = s & rg.Columns("B").Address
    s = s & ErrS()
    s = s & rg.Rows(4).Address
    s = s & ErrS()
    s = s & rg.Columns("D").Address
    s = s & ErrS()
    s = s & rg.Rows(0).Address
    s = s & ErrS()
    s = s & rg.EntireRow.Address
    s = s & rg.EntireColumn.Address
    s = s & rg.Rows.Count
    s = s & rg.Cells.Count
    s = s & rg.Count
    s = s & rg.CountLarge
    s = s & rg.Row
    s = s & rg.Column
    s = s & rg.Areas.Count
    s = s & rg.Worksheet.Name
    s = s & rg.Worksheet.Name
    s = s & TypeName(rg.Application)
    s = s & rg.Cells.Rows.Count
    s = s & ErrS()
    R = s
Done:
    M96 = R
    Exit Function
EH:
    M96 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M97() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:C3").Value = 1
    ws.Range("B2").Value = Empty
    On Error Resume Next
    s = ws.Range("A1:C3").Cells.SpecialCells(xlCellTypeBlanks).Address
    s = s & ws.Range("A1:C3").Rows(2).Cells(2).Address
    s = s & ErrS()
    ws.Range("A1:C3").Replace "1", "9": s = s & ws.Range("A1").Value & ErrS()
    ws.Range("A1:C3").Replace What:="9", Replacement:="x", LookAt:=xlWhole, MatchCase:=True: s = s & ws.Range("A1").Value & ErrS()
    s = s & ws.Range("A1:C3").Replace("x", "y")
    s = s & ErrS()
    s = s & ws.Range("A1:C3").Replace("q", "y")
    s = s & ErrS()
    s = s & ws.Range("A1:C3").Replace(Null, "x")
    s = s & ErrS()
    s = s & ws.Range("A1:C3").Replace("y", Null)
    s = s & ErrS()
    s = s & Dmp(ws.Range("A1").Value)
    s = s & ws.Range("A1:C3").Replace(What:="y", Replacement:=5, LookAt:=99) & ErrS()
    R = s
Done:
    M97 = R
    Exit Function
EH:
    M97 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M98() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:B2").Value = 1
    ws.Range("D4").Value = 1
    On Error Resume Next
    s = ws.UsedRange.Address
    s = s & ws.Cells.SpecialCells(xlCellTypeLastCell).Address
    ws.Range("D4").Clear: s = s & ws.UsedRange.Address & ws.Cells.SpecialCells(xlCellTypeLastCell).Address
    ws.Range("D4").Font.Bold = True: s = s & ws.UsedRange.Address
    ws.Range("D4").ClearFormats: s = s & ws.UsedRange.Address
    ws.Rows(10).RowHeight = 30: s = s & ws.UsedRange.Address
    ws.Columns(9).ColumnWidth = 20: s = s & ws.UsedRange.Address
    ws.Cells.Clear: s = s & ws.UsedRange.Address & ws.Cells.SpecialCells(xlCellTypeLastCell).Address
    ws.Range("C3").Value = "": s = s & ws.UsedRange.Address
    ws.Range("C3").Value = Empty: s = s & ws.UsedRange.Address
    ws.Range("C3").Formula = "": s = s & ws.UsedRange.Address
    ws.Range("E5").NumberFormat = "0.00": s = s & ws.UsedRange.Address
    ws.Range("E5").Value = 0: ws.Range("E5").ClearContents: s = s & ws.UsedRange.Address
    s = s & ws.UsedRange.Rows.Count
    s = s & ws.UsedRange.Columns.Count
    R = s
Done:
    M98 = R
    Exit Function
EH:
    M98 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M99() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:C3").Value = 1
    On Error Resume Next
    ws.Range("A1:C3").Merge: s = s & ws.Range("B2").MergeCells & ws.Range("B2").MergeArea.Address & ws.Range("A1").MergeArea.Address & Dmp(ws.Range("A1:C3").MergeCells) & Dmp(ws.Range("A1:D3").MergeCells)
    ws.Range("B2").Value = 5: s = s & ErrS() & ws.Range("A1").Value
    ws.Range("A1:C3").UnMerge: s = s & ws.Range("B2").MergeCells & ErrS() & Dmp(ws.Range("B2").Value)
    ws.Range("A1:C3").Merge True: s = s & ws.Range("A1").MergeArea.Address & ws.Range("A2").MergeArea.Address & ErrS()
    ws.Range("A1:C3").UnMerge
    ws.Range("A1:B2").Merge: ws.Range("B2:C3").Merge: s = s & ErrS()
    ws.Range("A1").Merge: s = s & ErrS() & ws.Range("A1").MergeCells
    s = s & ws.Range("A1:A1").MergeArea.Address
    s = s & ErrS()
    R = s
Done:
    M99 = R
    Exit Function
EH:
    M99 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M100() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, rg As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:B2").Value = 3
    On Error Resume Next
    s = s & ws.Range("A1:B2").Clear
    s = s & TypeName(ws.Range("A1").ClearContents)
    s = s & TypeName(ws.Range("A1").Select)
    s = s & TypeName(ws.Range("A1").Activate)
    s = s & ErrS()
    ws.Range("A1:B2").Select: s = s & ErrS() & Application.Selection.Address & ActiveCell.Address & ErrS()
    ws.Range("A1:B2").Activate: s = s & ErrS() & Application.Selection.Address & ActiveCell.Address
    ws.Range("D1").Activate: s = s & Application.Selection.Address & ActiveCell.Address & ErrS()
    ws.Range("A1,C3").Select: s = s & Application.Selection.Address & ActiveCell.Address & ErrS()
    ws.Range("A1,C3").Activate: s = s & ErrS()
    Worksheets("Sheet2").Range("A1").Select: s = s & ErrS()
    Worksheets("Sheet2").Activate: ws.Range("A1").Select: s = s & ErrS()
    ws.Activate: ws.Range("E5:F6").Select: ws.Range("E5").Activate: s = s & ActiveCell.Address & Selection.Address & ErrS()
    ws.Range("F6").Activate: s = s & ActiveCell.Address & Selection.Address & ErrS()
    ws.Range("G7").Activate: s = s & ActiveCell.Address & Selection.Address & ErrS()
    ws.Cells.Select: ActiveCell.Activate: s = s & Selection.Address & ActiveCell.Address
    ws.Range("A:A").Select: s = s & ActiveCell.Address & Selection.Address & TypeName(Selection) & Selection.Count
    ws.Range("A1").Select: Selection.Offset(1, 1).Select: s = s & ActiveCell.Address
    ActiveCell.Resize(2, 2).Select: s = s & Selection.Address & ActiveCell.Address
    R = s
Done:
    M100 = R
    Exit Function
EH:
    M100 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M101() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    On Error Resume Next
    s = Application.Name
    s = s & ""
    s = s & Application.Version
    s = s & Application.Build
    s = s & Application.OperatingSystem
    s = s & Application.International(xlDecimalSeparator)
    s = s & Application.International(xlListSeparator)
    s = s & Application.International(xlCountryCode)
    s = s & Application.International(xlThousandsSeparator)
    s = s & Application.International(xlDateOrder)
    s = s & Application.International(xlCurrencyCode)
    s = s & Application.International(99)
    s = s & ErrS()
    s = s & Application.International(0)
    s = s & ErrS()
    s = s & Application.International("a")
    s = s & ErrS()
    s = s & Dmp(Application.Calculation)
    s = s & Dmp(Application.CalculationState)
    s = s & Dmp(Application.Iteration)
    s = s & Dmp(Application.MaxIterations)
    s = s & Dmp(Application.MaxChange)
    s = s & Dmp(Application.PrecisionAsDisplayed)
    s = s & Dmp(Application.ReferenceStyle)
    s = s & Dmp(Application.UseSystemSeparators)
    s = s & Dmp(Application.DecimalSeparator)
    s = s & Dmp(Application.ThousandsSeparator)
    s = s & Dmp(Application.EnableEvents)
    s = s & Dmp(Application.ScreenUpdating)
    s = s & Dmp(Application.DisplayAlerts)
    s = s & Dmp(Application.Interactive)
    s = s & Dmp(Application.StatusBar)
    s = s & Dmp(Application.CutCopyMode)
    s = s & Dmp(Application.AutoCorrect.ReplaceText)
    s = s & Dmp(Application.CalculateBeforeSave)
    s = s & Dmp(Application.DefaultFilePath <> "")
    s = s & Dmp(Application.EditDirectlyInCell)
    s = s & Dmp(Application.FixedDecimal)
    Application.FixedDecimalPlaces = 2
    s = s & Dmp(Application.FixedDecimalPlaces)
    s = s & Dmp(Application.MoveAfterReturn)
    s = s & Dmp(Application.MoveAfterReturnDirection)
    s = s & Dmp(Application.DisplayFormulaBar)
    s = s & Dmp(Application.AskToUpdateLinks)
    s = s & Dmp(Application.AutomationSecurity)
    s = s & Dmp(Application.Cursor)
    s = s & Dmp(Application.DisplayStatusBar)
    s = s & Dmp(Application.Calculation = xlCalculationAutomatic)
    s = s & ErrS()
    R = s
Done:
    M101 = R
    Exit Function
EH:
    M101 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M102() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Application.MaxIterations = 0: s = s & ErrS() & Application.MaxIterations
    Application.MaxIterations = 32768: s = s & ErrS() & Application.MaxIterations
    Application.MaxIterations = 1.5: s = s & ErrS() & Application.MaxIterations
    Application.MaxIterations = "5": s = s & ErrS() & Application.MaxIterations
    Application.MaxIterations = 100
    Application.MaxChange = -1: s = s & ErrS() & Application.MaxChange
    Application.MaxChange = 0: s = s & ErrS() & Application.MaxChange
    Application.MaxChange = 0.001
    Application.Calculation = 5: s = s & ErrS() & Application.Calculation
    Application.Calculation = xlCalculationSemiautomatic: s = s & ErrS() & Application.Calculation
    Application.Calculation = xlCalculationAutomatic
    Application.FixedDecimalPlaces = 16: s = s & ErrS()
    Application.FixedDecimalPlaces = -1: s = s & ErrS()
    Application.FixedDecimalPlaces = 0: s = s & ErrS()
    Application.FixedDecimalPlaces = 2
    Application.ReferenceStyle = 3: s = s & ErrS()
    Application.StatusBar = 5: s = s & ErrS() & Dmp(Application.StatusBar)
    Application.StatusBar = False
    Application.DisplayAlerts = 5: s = s & ErrS() & Dmp(Application.DisplayAlerts)
    Application.DisplayAlerts = False
    Application.ScreenUpdating = "a": s = s & ErrS()
    Application.EnableEvents = Null: s = s & ErrS()
    Application.EnableEvents = True
    Application.Interactive = False
    R = s
Done:
    M102 = R
    Exit Function
EH:
    M102 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M103() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    On Error Resume Next
    s = Dmp(ws.Range("A1").Value)
    s = s & Dmp(ws.Range("A1").Text)
    s = s & Dmp(ws.Range("A1").Formula)
    s = s & Dmp(ws.Range("A1").Interior.ColorIndex)
    s = s & Dmp(ws.Range("A1").Font.Name)
    s = s & Dmp(ws.Range("A1").Font.Size)
    s = s & Dmp(ws.Range("A1").HorizontalAlignment)
    s = s & Dmp(ws.Range("A1").VerticalAlignment)
    s = s & Dmp(ws.Range("A1").WrapText)
    s = s & Dmp(ws.Range("A1").ColumnWidth)
    s = s & Dmp(ws.Range("A1").RowHeight)
    s = s & Dmp(ws.Range("A1").Width)
    s = s & Dmp(ws.Range("A1").Height)
    s = s & Dmp(ws.Range("A1").Left)
    s = s & Dmp(ws.Range("A1").Top)
    s = s & Dmp(ws.Range("A1").Locked)
    s = s & Dmp(ws.Range("A1").FormulaHidden)
    s = s & Dmp(ws.Range("A1").IndentLevel)
    s = s & Dmp(ws.Range("A1").Orientation)
    s = s & Dmp(ws.Range("A1").ShrinkToFit)
    s = s & Dmp(ws.Range("A1").Style.Name)
    s = s & Dmp(ws.Range("A1").PrefixCharacter)
    s = s & Dmp(ws.Range("A1").ID)
    s = s & Dmp(ws.Range("A1").Hyperlinks.Count)
    s = s & Dmp(ws.Range("A1").Validation.Type)
    s = s & ErrS()
    s = s & Dmp(ws.Range("A1").Borders.LineStyle)
    s = s & Dmp(ws.Range("A1").Borders(xlEdgeLeft).LineStyle)
    s = s & Dmp(ws.Range("A1").Borders(99).LineStyle)
    s = s & ErrS()
    s = s & Dmp(ws.Range("A1").ReadingOrder)
    s = s & Dmp(ws.Range("A1").AddIndent)
    s = s & Dmp(ws.Range("A1").NumberFormat)
    s = s & Dmp(ws.Range("A1").MergeCells)
    s = s & Dmp(ws.Range("A1").ListHeaderRows)
    s = s & Dmp(ws.Range("A1").PageBreak)
    s = s & Dmp(ws.Range("A1").ShowDetail)
    s = s & Dmp(ws.Range("A1").OutlineLevel)
    s = s & Dmp(ws.Range("A1").SoundNote.Text)
    s = s & ErrS()
    R = s
Done:
    M103 = R
    Exit Function
EH:
    M103 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M104() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:B2").Value = 1
    ws.Range("B2").Font.Bold = True
    ws.Range("A1").Interior.Color = 255
    On Error Resume Next
    s = Dmp(ws.Range("A1:B2").Font.Bold)
    s = s & Dmp(ws.Range("A1:B2").Interior.Color)
    s = s & Dmp(ws.Range("A1:B2").Interior.ColorIndex)
    s = s & Dmp(ws.Range("A1:B2").Font.Name)
    s = s & Dmp(ws.Range("A1:B2").Locked)
    s = s & Dmp(ws.Range("A1:B2").HorizontalAlignment)
    s = s & Dmp(ws.Range("A1:B2").NumberFormat)
    s = s & Dmp(ws.Range("A1:B2").Text)
    s = s & Dmp(ws.Range("A1:B2").RowHeight)
    s = s & Dmp(ws.Range("A1:B2").ColumnWidth)
    s = s & Dmp(ws.Range("A1:B2").Borders.LineStyle)
    s = s & Dmp(ws.Range("A1:B2").Font.Color)
    s = s & Dmp(ws.Range("A1:B2").Interior.Pattern)
    ws.Range("A1").Interior.Color = 16777216: s = s & ErrS()
    ws.Range("A1").Interior.Color = -1: s = s & ErrS()
    ws.Range("A1").Interior.ColorIndex = 57: s = s & ErrS()
    ws.Range("A1").Interior.ColorIndex = 0: s = s & ErrS()
    ws.Range("A1").Interior.ColorIndex = xlColorIndexNone: s = s & ErrS() & Dmp(ws.Range("A1").Interior.Color)
    ws.Range("A1").Font.Size = 0: s = s & ErrS()
    ws.Range("A1").Font.Size = 410: s = s & ErrS()
    ws.Range("A1").Font.Size = 409.5: s = s & ErrS() & ws.Range("A1").Font.Size
    ws.Range("A1").Font.Size = 10.25: s = s & ErrS() & ws.Range("A1").Font.Size
    ws.Range("A1").Font.Name = "": s = s & ErrS()
    ws.Range("A1").Font.Name = String(32, "a"): s = s & ErrS() & ws.Range("A1").Font.Name
    ws.Range("A1").HorizontalAlignment = 99: s = s & ErrS()
    ws.Range("A1").HorizontalAlignment = xlCenter: s = s & ErrS() & ws.Range("A1").HorizontalAlignment
    ws.Range("A1").VerticalAlignment = xlJustify: s = s & ErrS()
    ws.Range("A1").Orientation = 91: s = s & ErrS()
    ws.Range("A1").Orientation = -90: s = s & ErrS() & ws.Range("A1").Orientation
    ws.Range("A1").Orientation = xlVertical: s = s & ErrS() & ws.Range("A1").Orientation
    ws.Range("A1").IndentLevel = 16: s = s & ErrS()
    ws.Range("A1").IndentLevel = 15: s = s & ErrS() & ws.Range("A1").IndentLevel
    ws.Range("A1").RowHeight = 410: s = s & ErrS()
    ws.Range("A1").RowHeight = 409.5: s = s & ErrS() & ws.Range("A1").RowHeight
    ws.Range("A1").RowHeight = 0: s = s & ErrS() & ws.Range("A1").RowHeight & ws.Rows(1).Hidden
    ws.Range("A1").RowHeight = -1: s = s & ErrS()
    ws.Range("A1").ColumnWidth = 256: s = s & ErrS()
    ws.Range("A1").ColumnWidth = 255: s = s & ErrS() & ws.Range("A1").ColumnWidth
    ws.Range("A1").ColumnWidth = -1: s = s & ErrS()
    ws.Range("A1").ColumnWidth = 0: s = s & ErrS() & ws.Columns(1).Hidden
    R = s
Done:
    M104 = R
    Exit Function
EH:
    M104 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M105() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    On Error Resume Next
    s = ws.Name
    s = s & ws.Index
    s = s & ws.CodeName
    s = s & ws.Visible
    s = s & ws.ProtectContents
    s = s & ws.ProtectDrawingObjects
    s = s & ws.ProtectScenarios
    s = s & ws.ProtectionMode
    s = s & ws.EnableCalculation
    s = s & ws.EnableSelection
    s = s & ws.StandardWidth
    s = s & ws.StandardHeight
    s = s & ws.Type
    s = s & ws.Next.Name
    s = s & ErrS()
    s = s & ws.Previous.Name
    s = s & ErrS()
    s = s & Dmp(ws.Next.Next.Name)
    s = s & ErrS()
    s = s & ws.Names.Count
    s = s & ws.Shapes.Count
    s = s & ws.ChartObjects.Count
    s = s & ws.ListObjects.Count
    s = s & ws.PivotTables.Count
    s = s & ws.Comments.Count
    s = s & ws.Hyperlinks.Count
    s = s & ws.OLEObjects.Count
    s = s & ws.AutoFilterMode
    s = s & ws.FilterMode
    s = s & ws.DisplayPageBreaks
    s = s & ws.TransitionExpEval
    s = s & ws.ScrollArea
    s = s & ws.Tab.ColorIndex
    s = s & ErrS()
    s = s & ws.PageSetup.Orientation
    s = s & ws.PageSetup.PaperSize
    s = s & ws.PageSetup.Zoom
    s = s & ws.PageSetup.LeftMargin
    s = s & ErrS()
    s = s & ws.PageSetup.PrintArea
    s = s & ws.PageSetup.CenterHeader
    s = s & ws.PageSetup.Order
    s = s & ErrS()
    s = s & ws.Cells.Count
    s = s & ws.UsedRange.Address
    s = s & ws.Rows.Count
    s = s & ws.Range("A1").Worksheet.Name
    ws.EnableSelection = 5: s = s & ErrS()
    ws.ScrollArea = "A1:B2": s = s & ws.ScrollArea & ErrS()
    ws.ScrollArea = "": s = s & ErrS()
    ws.StandardWidth = 0: s = s & ErrS()
    ws.StandardWidth = 8.43: s = s & ErrS() & ws.StandardWidth
    ws.Tab.ColorIndex = 5: s = s & ErrS() & ws.Tab.ColorIndex & ws.Tab.Color
    ws.Tab.Color = 255: s = s & ErrS() & ws.Tab.Color
    ws.Tab.ColorIndex = xlColorIndexNone
    ws.PageSetup.Zoom = 5: s = s & ErrS()
    ws.PageSetup.Zoom = 10: s = s & ErrS() & ws.PageSetup.Zoom
    ws.PageSetup.Zoom = False
    ws.PageSetup.PrintArea = "A1:B2": s = s & ErrS() & ws.PageSetup.PrintArea
    ws.PageSetup.PrintArea = "": s = s & ErrS() & ws.PageSetup.PrintArea
    ws.PageSetup.PrintArea = "Z": s = s & ErrS()
    ws.PageSetup.LeftMargin = -1: s = s & ErrS()
    ws.PageSetup.LeftMargin = 36.5: s = s & ErrS() & ws.PageSetup.LeftMargin
    R = s
Done:
    M105 = R
    Exit Function
EH:
    M105 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M106() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, wb As Workbook
    Set wb = ThisWorkbook
    On Error Resume Next
    s = wb.Saved
    s = s & wb.Path
    s = s & wb.HasVBProject
    s = s & wb.FileFormat
    s = s & wb.ReadOnly
    s = s & wb.Date1904
    s = s & wb.Sheets.Count
    s = s & wb.Windows.Count
    s = s & wb.Styles.Count
    s = s & wb.Names.Count
    s = s & wb.CodeName
    s = s & wb.ProtectStructure
    s = s & wb.ProtectWindows
    s = s & wb.IsAddin
    s = s & wb.PrecisionAsDisplayed
    s = s & wb.AcceptLabelsInFormulas
    s = s & wb.CalculationVersion
    s = s & wb.ForceFullCalculation
    s = s & wb.AutoSaveOn
    s = s & wb.HasPassword
    s = s & wb.MultiUserEditing
    s = s & wb.UpdateLinks
    s = s & wb.EnableAutoRecover
    s = s & wb.Final
    s = s & wb.Password
    s = s & ErrS()
    s = s & wb.ActiveSheet.Name
    s = s & wb.ActiveChart.Name
    s = s & ErrS()
    s = s & wb.Worksheets.Count
    s = s & wb.Charts.Count
    s = s & wb.CustomDocumentProperties.Count
    s = s & wb.BuiltinDocumentProperties("Title")
    s = s & ErrS()
    s = s & wb.BuiltinDocumentProperties("nope")
    s = s & ErrS()
    s = s & wb.Date1904
    s = s & wb.LinkSources(1)
    s = s & ErrS()
    s = s & wb.Colors(1)
    s = s & wb.Colors(56)
    s = s & wb.Colors(57)
    s = s & ErrS()
    s = s & wb.Colors(0)
    s = s & ErrS()
    wb.Date1904 = True: s = s & ErrS() & wb.Date1904 & Application.Evaluate("DATE(1900,1,1)") & Application.Evaluate("A1")
    wb.Date1904 = False
    wb.PrecisionAsDisplayed = False
    wb.Saved = 5: s = s & ErrS() & wb.Saved
    wb.Saved = True
    R = s
Done:
    M106 = R
    Exit Function
EH:
    M106 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M107() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, ws As Worksheet, c As Range
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("A1:A5").Value = Application.Transpose(Array(3, 1, 2, "b", "a"))
    On Error Resume Next
    s = Dmp(Application.WorksheetFunction.Min(ws.Range("A1:A5")))
    s = s & Dmp(Application.Min(ws.Range("A1:A5")))
    s = s & Dmp(Application.CountA(ws.Range("A1:A5")))
    s = s & Dmp(Application.Count(ws.Range("A1:A5")))
    s = s & Dmp(Application.Average(ws.Range("A1:A3")))
    s = s & Dmp(Application.Average(ws.Range("A4:A5")))
    s = s & Dmp(Application.Sum(ws.Range("A4:A5")))
    s = s & Dmp(Application.Match("a", ws.Range("A1:A5"), 0))
    s = s & Dmp(Application.Match("z", ws.Range("A1:A5"), 0))
    s = s & Dmp(Application.VLookup("b", ws.Range("A1:A5"), 2, False))
    s = s & Dmp(Application.VLookup("b", ws.Range("A1:A5"), 1, False))
    s = s & Dmp(Application.WorksheetFunction.VLookup("z", ws.Range("A1:A5"), 1, False))
    s = s & ErrS()
    s = s & Dmp(Application.WorksheetFunction.Match("z", ws.Range("A1:A5"), 0))
    s = s & ErrS()
    s = s & Dmp(Evaluate("SUM(A1:A3)")) & Dmp(ws.Evaluate("SUM(A1:A3)")) & Dmp(ws.Evaluate("A1:A3")) & Dmp([A1]) & Dmp([A1:A2]) & Dmp([SUM(A1:A3)]) & Dmp(ws.[A1].Address) & ErrS()
    R = s
Done:
    M107 = R
    Exit Function
EH:
    M107 = "#ERR " & Err.Number & " " & Err.Description
End Function
