Attribute VB_Name = "VbFz1Mac"
Option Explicit
Public Sub FzMac0()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(1, 1).Value = "TRUE"
    v = ws.Cells(1, 1).Value: ws.Cells(1, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Cells(2, 1).Value = "012"
    v = ws.Cells(2, 1).Value: ws.Cells(2, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A3:B3").Value = Array("x", CCur(1.5))
    ws.Range("E3").Value = Application.WorksheetFunction.Sum(ws.Range("A3:B3")) + Err.Number
    Err.Clear
    ws.Range("A4:B4").Value = 5
    ws.Range("E4").Value = Application.WorksheetFunction.Sum(ws.Range("A4:B4")) + Err.Number
    Err.Clear
    ws.Range("A5").Value = "012"
    ws.Range("B5").Formula = "=A5+1"
    ws.Range("E5").Value = ws.Range("B5").Value + 0.5
    Err.Clear
End Sub
Public Sub FzMac1()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A6").Value = True
    ws.Range("B6").Formula = "=A6+1"
    ws.Range("E6").Value = ws.Range("B6").Value + 0.5
    Err.Clear
    ws.Range("A7").Formula = "#N/A"
    ws.Range("B7").Value = TypeName(ws.Range("A7").Value) & "|" & ws.Range("A7").NumberFormat & "|" & Err.Number
    Err.Clear
    a(1, 1) = "TRUE": a(1, 2) = "TRUE": a(2, 1) = "#N/A": a(2, 2) = False
    ws.Range("C8:D8").Resize(2, 2).Value = a
    a(1, 1) = #1/2/2000 3:04:05 PM#: a(1, 2) = CLng(-5): a(2, 1) = -0#: a(2, 2) = 9.99999999999999E+307
    ws.Range("C9:D9").Resize(2, 2).Value = a
    ws.Range("A10").Formula = CSng(0.1)
    ws.Range("B10").Value = TypeName(ws.Range("A10").Value) & "|" & ws.Range("A10").NumberFormat & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac2()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(11, 1).Value = True
    v = ws.Cells(11, 1).Value: ws.Cells(11, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A12:B12").Value = Array("x", CCur(1.5))
    ws.Range("E12").Value = Application.WorksheetFunction.Sum(ws.Range("A12:B12")) + Err.Number
    Err.Clear
    ws.Range("A13").Value = False
    ws.Range("B13").Formula = "=A13&"""
    ws.Range("E13").Value = ws.Range("B13").Value + "1"
    Err.Clear
    ws.Range("A14:B14").Value = Array("x", CCur(1.5))
    ws.Range("E14").Value = Application.WorksheetFunction.Sum(ws.Range("A14:B14")) + Err.Number
    Err.Clear
    a(1, 1) = "'x": a(1, 2) = "1e3": a(2, 1) = CInt(7): a(2, 2) = Empty
    ws.Range("C15:D15").Resize(2, 2).Value = a
End Sub
Public Sub FzMac3()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A16:B16").Value = 5
    ws.Range("E16").Value = Application.WorksheetFunction.Sum(ws.Range("A16:B16")) + Err.Number
    Err.Clear
    ws.Range("A17:B17").Value = Array("x", CCur(1.5))
    ws.Range("E17").Value = Application.WorksheetFunction.Sum(ws.Range("A17:B17")) + Err.Number
    Err.Clear
    a(1, 1) = True: a(1, 2) = 9.99999999999999E+307: a(2, 1) = "": a(2, 2) = 2147483648#
    ws.Range("C18:D18").Resize(2, 2).Value = a
    ws.Range("A19:B19").Value = Array(1, 2)
    ws.Range("E19").Value = Application.WorksheetFunction.Sum(ws.Range("A19:B19")) + Err.Number
    Err.Clear
    ws.Range("A20").Value = CDbl(CSng(1.1))
    ws.Range("B20").Value = TypeName(ws.Range("A20").Value) & "|" & ws.Range("A20").NumberFormat & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac4()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A21").Formula = CVErr(2042)
    ws.Range("B21").Value = TypeName(ws.Range("A21").Value) & "|" & ws.Range("A21").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A22").Value = "#N/A"
    ws.Range("B22").Formula = "=A22=1"
    ws.Range("E22").Value = ws.Range("B22").Value + 0.5
    Err.Clear
    ws.Cells(23, 1).Value = "1/2/2000"
    v = ws.Cells(23, 1).Value: ws.Cells(23, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Cells(24, 1).Value = CCur(-0.005)
    v = ws.Cells(24, 1).Value: ws.Cells(24, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Cells(25, 1).Value = CByte(200)
    v = ws.Cells(25, 1).Value: ws.Cells(25, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac5()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A26").Value2 = 1.5
    ws.Range("B26").Value = TypeName(ws.Range("A26").Value) & "|" & ws.Range("A26").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A27").Value = 9.99999999999999E+307
    ws.Range("B27").Value = TypeName(ws.Range("A27").Value) & "|" & ws.Range("A27").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Cells(28, 1).Value = CByte(200)
    v = ws.Cells(28, 1).Value: ws.Cells(28, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A29").Value = Array(CCur(2.5), #1/1/2001#)
    ws.Range("B29").Value = TypeName(ws.Range("A29").Value) & "|" & ws.Range("A29").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Cells(30, 1).Value = Array(1, "a", True)
    v = ws.Cells(30, 1).Value: ws.Cells(30, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac6()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A31").Value = "#N/A"
    ws.Range("B31").Formula = "=A31+1"
    ws.Range("E31").Value = ws.Range("B31").Value + 1
    Err.Clear
    ws.Cells(32, 1).Value = #6:00:00 AM#
    v = ws.Cells(32, 1).Value: ws.Cells(32, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A33").Value2 = CVErr(2042)
    ws.Range("B33").Value = TypeName(ws.Range("A33").Value) & "|" & ws.Range("A33").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A34").Value = CVErr(2007)
    ws.Range("B34").Formula = "=A34=1"
    ws.Range("E34").Value = ws.Range("B34").Value + "1"
    Err.Clear
    ws.Cells(35, 1).Value = CDec("1.2345678901234567890")
    v = ws.Cells(35, 1).Value: ws.Cells(35, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac7()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(36, 1).Value = -0#
    v = ws.Cells(36, 1).Value: ws.Cells(36, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A37").Value = 1.5
    ws.Range("B37").Value = TypeName(ws.Range("A37").Value) & "|" & ws.Range("A37").NumberFormat & "|" & Err.Number
    Err.Clear
    a(1, 1) = CVErr(2007): a(1, 2) = CCur(1.23456): a(2, 1) = #1/2/2000 3:04:05 PM#: a(2, 2) = 1.5
    ws.Range("C38:D38").Resize(2, 2).Value = a
    ws.Range("A39").Value = "012"
    ws.Range("B39").Value = TypeName(ws.Range("A39").Value) & "|" & ws.Range("A39").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A40").Value = #1/2/2000 3:04:05 PM#
    ws.Range("B40").Formula = "=A40*2"
    ws.Range("E40").Value = ws.Range("B40").Value + "1"
    Err.Clear
End Sub
Public Sub FzMac8()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    a(1, 1) = 0.1 + 0.2: a(1, 2) = CDec("1.2345678901234567890"): a(2, 1) = Null: a(2, 2) = CInt(7)
    ws.Range("C41:D41").Resize(2, 2).Value = a
    ws.Cells(42, 1).Value = 9.99999999999999E+307
    v = ws.Cells(42, 1).Value: ws.Cells(42, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A43:B43").Value = Array(1, 2)
    ws.Range("E43").Value = Application.WorksheetFunction.Sum(ws.Range("A43:B43")) + Err.Number
    Err.Clear
    ws.Range("A44").Value = "  5 "
    ws.Range("B44").Formula = "=A44=1"
    ws.Range("E44").Value = ws.Range("B44").Value + 1
    Err.Clear
    ws.Range("A45").Value = "012"
    ws.Range("B45").Formula = "=A45*2"
    ws.Range("E45").Value = ws.Range("B45").Value + 0.5
    Err.Clear
End Sub
Public Sub FzMac9()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A46").Value = Null
    ws.Range("B46").Formula = "=A46=1"
    ws.Range("E46").Value = ws.Range("B46").Value + "1"
    Err.Clear
    ws.Range("A47:B47").Value = Array(Empty, Null)
    ws.Range("E47").Value = Application.WorksheetFunction.Sum(ws.Range("A47:B47")) + Err.Number
    Err.Clear
    a(1, 1) = "TRUE": a(1, 2) = #6:00:00 AM#: a(2, 1) = CVErr(2007): a(2, 2) = True
    ws.Range("C48:D48").Resize(2, 2).Value = a
    ws.Range("A49:B49").Value = Array(Empty, Null)
    ws.Range("E49").Value = Application.WorksheetFunction.Sum(ws.Range("A49:B49")) + Err.Number
    Err.Clear
    ws.Range("A50:B50").Value = 5
    ws.Range("E50").Value = Application.WorksheetFunction.Sum(ws.Range("A50:B50")) + Err.Number
    Err.Clear
End Sub
Public Sub FzMac10()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    a(1, 1) = 0.1 + 0.2: a(1, 2) = Null: a(2, 1) = CVErr(2007): a(2, 2) = True
    ws.Range("C51:D51").Resize(2, 2).Value = a
    ws.Cells(52, 1).Value = "012"
    v = ws.Cells(52, 1).Value: ws.Cells(52, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    a(1, 1) = "'x": a(1, 2) = "012": a(2, 1) = Null: a(2, 2) = "012"
    ws.Range("C53:D53").Resize(2, 2).Value = a
    ws.Range("A54:B54").Value = Array("x", CCur(1.5))
    ws.Range("E54").Value = Application.WorksheetFunction.Sum(ws.Range("A54:B54")) + Err.Number
    Err.Clear
    a(1, 1) = CDbl(CSng(1.1)): a(1, 2) = 9.99999999999999E+307: a(2, 1) = "TRUE": a(2, 2) = 0.1 + 0.2
    ws.Range("C55:D55").Resize(2, 2).Value = a
End Sub
Public Sub FzMac11()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    a(1, 1) = Null: a(1, 2) = Array(CCur(2.5), #1/1/2001#): a(2, 1) = CCur(-0.005): a(2, 2) = CByte(200)
    ws.Range("C56:D56").Resize(2, 2).Value = a
    a(1, 1) = #6:00:00 AM#: a(1, 2) = "1/2/2000": a(2, 1) = #6:00:00 AM#: a(2, 2) = 0.1 + 0.2
    ws.Range("C57:D57").Resize(2, 2).Value = a
    ws.Range("A58").Value = CDec("1.2345678901234567890")
    ws.Range("B58").Formula = "=A58+1"
    ws.Range("E58").Value = ws.Range("B58").Value + CCur(1)
    Err.Clear
    ws.Range("A59").Value2 = #6:00:00 AM#
    ws.Range("B59").Value = TypeName(ws.Range("A59").Value) & "|" & ws.Range("A59").NumberFormat & "|" & Err.Number
    Err.Clear
    a(1, 1) = "1/2/2000": a(1, 2) = CLng(-5): a(2, 1) = "#N/A": a(2, 2) = "012"
    ws.Range("C60:D60").Resize(2, 2).Value = a
End Sub
Public Sub FzMac12()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(61, 1).Value = CLng(-5)
    v = ws.Cells(61, 1).Value: ws.Cells(61, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A62").Value = ""
    ws.Range("B62").Value = TypeName(ws.Range("A62").Value) & "|" & ws.Range("A62").NumberFormat & "|" & Err.Number
    Err.Clear
    a(1, 1) = "": a(1, 2) = #1/2/2000 3:04:05 PM#: a(2, 1) = CVErr(2007): a(2, 2) = "012"
    ws.Range("C63:D63").Resize(2, 2).Value = a
    a(1, 1) = CDbl(CSng(1.1)): a(1, 2) = "#N/A": a(2, 1) = 0.1 + 0.2: a(2, 2) = CByte(200)
    ws.Range("C64:D64").Resize(2, 2).Value = a
    ws.Range("A65:B65").Value = Array("x", CCur(1.5))
    ws.Range("E65").Value = Application.WorksheetFunction.Sum(ws.Range("A65:B65")) + Err.Number
    Err.Clear
End Sub
Public Sub FzMac13()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(66, 1).Value = CInt(7)
    v = ws.Cells(66, 1).Value: ws.Cells(66, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A67:B67").Value = Array("x", CCur(1.5))
    ws.Range("E67").Value = Application.WorksheetFunction.Sum(ws.Range("A67:B67")) + Err.Number
    Err.Clear
    a(1, 1) = "#N/A": a(1, 2) = CLng(-5): a(2, 1) = "012": a(2, 2) = ""
    ws.Range("C68:D68").Resize(2, 2).Value = a
    ws.Range("A69").Value = "1/2/2000"
    ws.Range("B69").Formula = "=A69"
    ws.Range("E69").Value = ws.Range("B69").Value + CCur(1)
    Err.Clear
    a(1, 1) = CVErr(2042): a(1, 2) = #6:00:00 AM#: a(2, 1) = 1.5: a(2, 2) = CDec("1.2345678901234567890")
    ws.Range("C70:D70").Resize(2, 2).Value = a
End Sub
Public Sub FzMac14()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A71").Value = "1/2/2000"
    ws.Range("B71").Formula = "=A71"
    ws.Range("E71").Value = ws.Range("B71").Value + "1"
    Err.Clear
    ws.Range("A72:B72").Value = Array("x", CCur(1.5))
    ws.Range("E72").Value = Application.WorksheetFunction.Sum(ws.Range("A72:B72")) + Err.Number
    Err.Clear
    a(1, 1) = False: a(1, 2) = CByte(200): a(2, 1) = #6:00:00 AM#: a(2, 2) = CInt(7)
    ws.Range("C73:D73").Resize(2, 2).Value = a
    a(1, 1) = CByte(200): a(1, 2) = CByte(200): a(2, 1) = CInt(7): a(2, 2) = "#N/A"
    ws.Range("C74:D74").Resize(2, 2).Value = a
    ws.Range("A75").Value = CCur(-0.005)
    ws.Range("B75").Value = TypeName(ws.Range("A75").Value) & "|" & ws.Range("A75").NumberFormat & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac15()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    a(1, 1) = CInt(7): a(1, 2) = "=1+1": a(2, 1) = CCur(-0.005): a(2, 2) = CCur(-0.005)
    ws.Range("C76:D76").Resize(2, 2).Value = a
    ws.Range("A77:B77").Value = 5
    ws.Range("E77").Value = Application.WorksheetFunction.Sum(ws.Range("A77:B77")) + Err.Number
    Err.Clear
    ws.Range("A78").Value = CDec("1.2345678901234567890")
    ws.Range("B78").Formula = "=A78=1"
    ws.Range("E78").Value = ws.Range("B78").Value + 1
    Err.Clear
    ws.Cells(79, 1).Value = Array(CCur(2.5), #1/1/2001#)
    v = ws.Cells(79, 1).Value: ws.Cells(79, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    a(1, 1) = "#N/A": a(1, 2) = CVErr(2007): a(2, 1) = #6:00:00 AM#: a(2, 2) = "=1+1"
    ws.Range("C80:D80").Resize(2, 2).Value = a
End Sub
Public Sub FzMac16()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A81:B81").Value = Array("x", CCur(1.5))
    ws.Range("E81").Value = Application.WorksheetFunction.Sum(ws.Range("A81:B81")) + Err.Number
    Err.Clear
    ws.Cells(82, 1).Value = CLng(-5)
    v = ws.Cells(82, 1).Value: ws.Cells(82, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A83:B83").Value = 5
    ws.Range("E83").Value = Application.WorksheetFunction.Sum(ws.Range("A83:B83")) + Err.Number
    Err.Clear
    a(1, 1) = CDec("1.2345678901234567890"): a(1, 2) = "": a(2, 1) = "012": a(2, 2) = Empty
    ws.Range("C84:D84").Resize(2, 2).Value = a
    ws.Range("A85").Value = CCur(-0.005)
    ws.Range("B85").Formula = "=A85"
    ws.Range("E85").Value = ws.Range("B85").Value + "1"
    Err.Clear
End Sub
Public Sub FzMac17()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    a(1, 1) = Null: a(1, 2) = 9.99999999999999E+307: a(2, 1) = CLng(-5): a(2, 2) = 2147483648#
    ws.Range("C86:D86").Resize(2, 2).Value = a
    ws.Range("A87:B87").Value = CVErr(2015)
    ws.Range("E87").Value = Application.WorksheetFunction.Sum(ws.Range("A87:B87")) + Err.Number
    Err.Clear
    ws.Range("A88").Value2 = Array(1, "a", True)
    ws.Range("B88").Value = TypeName(ws.Range("A88").Value) & "|" & ws.Range("A88").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Cells(89, 1).Value = CCur(-0.005)
    v = ws.Cells(89, 1).Value: ws.Cells(89, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A90").Value = False
    ws.Range("B90").Formula = "=A90"
    ws.Range("E90").Value = ws.Range("B90").Value + 0.5
    Err.Clear
End Sub
Public Sub FzMac18()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A91").Value = #1/2/2000 3:04:05 PM#
    ws.Range("B91").Formula = "=A91&"""
    ws.Range("E91").Value = ws.Range("B91").Value + 0.5
    Err.Clear
    ws.Range("A92").Value = Empty
    ws.Range("B92").Formula = "=A92"
    ws.Range("E92").Value = ws.Range("B92").Value + 0.5
    Err.Clear
    ws.Range("A93:B93").Value = Array(Empty, Null)
    ws.Range("E93").Value = Application.WorksheetFunction.Sum(ws.Range("A93:B93")) + Err.Number
    Err.Clear
    ws.Range("A94:B94").Value = 5
    ws.Range("E94").Value = Application.WorksheetFunction.Sum(ws.Range("A94:B94")) + Err.Number
    Err.Clear
    a(1, 1) = CInt(7): a(1, 2) = "=1+1": a(2, 1) = -0#: a(2, 2) = True
    ws.Range("C95:D95").Resize(2, 2).Value = a
End Sub
Public Sub FzMac19()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A96:B96").Value = Array(Empty, Null)
    ws.Range("E96").Value = Application.WorksheetFunction.Sum(ws.Range("A96:B96")) + Err.Number
    Err.Clear
    ws.Range("A97").Value = Array(CCur(2.5), #1/1/2001#)
    ws.Range("B97").Formula = "=A97&"""
    ws.Range("E97").Value = ws.Range("B97").Value + 0.5
    Err.Clear
    ws.Cells(98, 1).Value = CSng(0.1)
    v = ws.Cells(98, 1).Value: ws.Cells(98, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A99:B99").Value = Array(Empty, Null)
    ws.Range("E99").Value = Application.WorksheetFunction.Sum(ws.Range("A99:B99")) + Err.Number
    Err.Clear
    ws.Range("A100").Value = Empty
    ws.Range("B100").Value = TypeName(ws.Range("A100").Value) & "|" & ws.Range("A100").NumberFormat & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac20()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(101, 1).Value = "#N/A"
    v = ws.Cells(101, 1).Value: ws.Cells(101, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A102").Value = 9.99999999999999E+307
    ws.Range("B102").Formula = "=A102"
    ws.Range("E102").Value = ws.Range("B102").Value + "1"
    Err.Clear
    ws.Range("A103").Value = "=1+1"
    ws.Range("B103").Formula = "=A103=1"
    ws.Range("E103").Value = ws.Range("B103").Value + "1"
    Err.Clear
    ws.Range("A104").Value = CVErr(2042)
    ws.Range("B104").Formula = "=A104&"""
    ws.Range("E104").Value = ws.Range("B104").Value + CCur(1)
    Err.Clear
    ws.Range("A105:B105").Value = Array(1, 2)
    ws.Range("E105").Value = Application.WorksheetFunction.Sum(ws.Range("A105:B105")) + Err.Number
    Err.Clear
End Sub
Public Sub FzMac21()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A106").Value2 = "'x"
    ws.Range("B106").Value = TypeName(ws.Range("A106").Value) & "|" & ws.Range("A106").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A107:B107").Value = CVErr(2015)
    ws.Range("E107").Value = Application.WorksheetFunction.Sum(ws.Range("A107:B107")) + Err.Number
    Err.Clear
    ws.Range("A108").Value = Empty
    ws.Range("B108").Formula = "=A108+1"
    ws.Range("E108").Value = ws.Range("B108").Value + "1"
    Err.Clear
    ws.Range("A109").Formula = "TRUE"
    ws.Range("B109").Value = TypeName(ws.Range("A109").Value) & "|" & ws.Range("A109").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A110").Value = CVErr(2042)
    ws.Range("B110").Value = TypeName(ws.Range("A110").Value) & "|" & ws.Range("A110").NumberFormat & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac22()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(111, 1).Value = CCur(-0.005)
    v = ws.Cells(111, 1).Value: ws.Cells(111, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A112").Value2 = #1/2/2000 3:04:05 PM#
    ws.Range("B112").Value = TypeName(ws.Range("A112").Value) & "|" & ws.Range("A112").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A113:B113").Value = Array("x", CCur(1.5))
    ws.Range("E113").Value = Application.WorksheetFunction.Sum(ws.Range("A113:B113")) + Err.Number
    Err.Clear
    ws.Range("A114").Formula = CCur(1.23456)
    ws.Range("B114").Value = TypeName(ws.Range("A114").Value) & "|" & ws.Range("A114").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A115:B115").Value = Array(1, 2)
    ws.Range("E115").Value = Application.WorksheetFunction.Sum(ws.Range("A115:B115")) + Err.Number
    Err.Clear
End Sub
Public Sub FzMac23()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A116").Value = False
    ws.Range("B116").Value = TypeName(ws.Range("A116").Value) & "|" & ws.Range("A116").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A117:B117").Value = CVErr(2015)
    ws.Range("E117").Value = Application.WorksheetFunction.Sum(ws.Range("A117:B117")) + Err.Number
    Err.Clear
    ws.Range("A118").Value = CCur(1.23456)
    ws.Range("B118").Formula = "=A118=1"
    ws.Range("E118").Value = ws.Range("B118").Value + "1"
    Err.Clear
    ws.Range("A119:B119").Value = 5
    ws.Range("E119").Value = Application.WorksheetFunction.Sum(ws.Range("A119:B119")) + Err.Number
    Err.Clear
    ws.Range("A120").Value = "  5 "
    ws.Range("B120").Formula = "=A120&"""
    ws.Range("E120").Value = ws.Range("B120").Value + "1"
    Err.Clear
End Sub
Public Sub FzMac24()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A121").Value2 = CDbl(CSng(1.1))
    ws.Range("B121").Value = TypeName(ws.Range("A121").Value) & "|" & ws.Range("A121").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A122:B122").Value = Array(1, 2)
    ws.Range("E122").Value = Application.WorksheetFunction.Sum(ws.Range("A122:B122")) + Err.Number
    Err.Clear
    ws.Range("A123:B123").Value = Array(Empty, Null)
    ws.Range("E123").Value = Application.WorksheetFunction.Sum(ws.Range("A123:B123")) + Err.Number
    Err.Clear
    a(1, 1) = Empty: a(1, 2) = CCur(-0.005): a(2, 1) = True: a(2, 2) = Array(1, "a", True)
    ws.Range("C124:D124").Resize(2, 2).Value = a
    ws.Range("A125").Value = CCur(-0.005)
    ws.Range("B125").Value = TypeName(ws.Range("A125").Value) & "|" & ws.Range("A125").NumberFormat & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac25()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A126").Value = Array(1, "a", True)
    ws.Range("B126").Formula = "=A126"
    ws.Range("E126").Value = ws.Range("B126").Value + "1"
    Err.Clear
    ws.Range("A127").Value = CInt(7)
    ws.Range("B127").Value = TypeName(ws.Range("A127").Value) & "|" & ws.Range("A127").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A128").Value = CDec("1.2345678901234567890")
    ws.Range("B128").Value = TypeName(ws.Range("A128").Value) & "|" & ws.Range("A128").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A129").Value = CCur(-0.005)
    ws.Range("B129").Value = TypeName(ws.Range("A129").Value) & "|" & ws.Range("A129").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Cells(130, 1).Value = 1.5
    v = ws.Cells(130, 1).Value: ws.Cells(130, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac26()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A131").Value = "  5 "
    ws.Range("B131").Formula = "=A131=1"
    ws.Range("E131").Value = ws.Range("B131").Value + 0.5
    Err.Clear
    ws.Range("A132:B132").Value = Array(Empty, Null)
    ws.Range("E132").Value = Application.WorksheetFunction.Sum(ws.Range("A132:B132")) + Err.Number
    Err.Clear
    ws.Range("A133").Value = CSng(0.1)
    ws.Range("B133").Formula = "=A133=1"
    ws.Range("E133").Value = ws.Range("B133").Value + 1
    Err.Clear
    ws.Range("A134").Value = "TRUE"
    ws.Range("B134").Value = TypeName(ws.Range("A134").Value) & "|" & ws.Range("A134").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A135:B135").Value = Array("x", CCur(1.5))
    ws.Range("E135").Value = Application.WorksheetFunction.Sum(ws.Range("A135:B135")) + Err.Number
    Err.Clear
End Sub
Public Sub FzMac27()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A136:B136").Value = CVErr(2015)
    ws.Range("E136").Value = Application.WorksheetFunction.Sum(ws.Range("A136:B136")) + Err.Number
    Err.Clear
    ws.Range("A137").Value = ""
    ws.Range("B137").Formula = "=A137&"""
    ws.Range("E137").Value = ws.Range("B137").Value + "1"
    Err.Clear
    ws.Range("A138:B138").Value = Array(1, 2)
    ws.Range("E138").Value = Application.WorksheetFunction.Sum(ws.Range("A138:B138")) + Err.Number
    Err.Clear
    a(1, 1) = "012": a(1, 2) = CInt(7): a(2, 1) = CLng(-5): a(2, 2) = 0.1 + 0.2
    ws.Range("C139:D139").Resize(2, 2).Value = a
    ws.Range("A140").Value = Array(CCur(2.5), #1/1/2001#)
    ws.Range("B140").Formula = "=A140=1"
    ws.Range("E140").Value = ws.Range("B140").Value + CCur(1)
    Err.Clear
End Sub
Public Sub FzMac28()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A141").Value = Array(CCur(2.5), #1/1/2001#)
    ws.Range("B141").Formula = "=A141*2"
    ws.Range("E141").Value = ws.Range("B141").Value + "1"
    Err.Clear
    a(1, 1) = CCur(1.23456): a(1, 2) = CDec("1.2345678901234567890"): a(2, 1) = CLng(-5): a(2, 2) = 9.99999999999999E+307
    ws.Range("C142:D142").Resize(2, 2).Value = a
    ws.Range("A143").Value = CInt(7)
    ws.Range("B143").Formula = "=A143&"""
    ws.Range("E143").Value = ws.Range("B143").Value + 1
    Err.Clear
    ws.Range("A144").Value = "1e3"
    ws.Range("B144").Value = TypeName(ws.Range("A144").Value) & "|" & ws.Range("A144").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A145").Value = False
    ws.Range("B145").Formula = "=A145*2"
    ws.Range("E145").Value = ws.Range("B145").Value + "1"
    Err.Clear
End Sub
Public Sub FzMac29()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(146, 1).Value = True
    v = ws.Cells(146, 1).Value: ws.Cells(146, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    a(1, 1) = "1/2/2000": a(1, 2) = Null: a(2, 1) = "TRUE": a(2, 2) = 0.1 + 0.2
    ws.Range("C147:D147").Resize(2, 2).Value = a
    ws.Range("A148").Value = CVErr(2042)
    ws.Range("B148").Formula = "=A148"
    ws.Range("E148").Value = ws.Range("B148").Value + 0.5
    Err.Clear
    ws.Range("A149:B149").Value = 5
    ws.Range("E149").Value = Application.WorksheetFunction.Sum(ws.Range("A149:B149")) + Err.Number
    Err.Clear
    ws.Range("A150").Value = 9.99999999999999E+307
    ws.Range("B150").Value = TypeName(ws.Range("A150").Value) & "|" & ws.Range("A150").NumberFormat & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac30()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(151, 1).Value = "1/2/2000"
    v = ws.Cells(151, 1).Value: ws.Cells(151, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A152").Value = -0#
    ws.Range("B152").Formula = "=A152*2"
    ws.Range("E152").Value = ws.Range("B152").Value + 1
    Err.Clear
    ws.Range("A153").Value = 1.5
    ws.Range("B153").Value = TypeName(ws.Range("A153").Value) & "|" & ws.Range("A153").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A154:B154").Value = Array(Empty, Null)
    ws.Range("E154").Value = Application.WorksheetFunction.Sum(ws.Range("A154:B154")) + Err.Number
    Err.Clear
    a(1, 1) = "TRUE": a(1, 2) = Array(1, "a", True): a(2, 1) = True: a(2, 2) = "012"
    ws.Range("C155:D155").Resize(2, 2).Value = a
End Sub
Public Sub FzMac31()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A156").Value = False
    ws.Range("B156").Value = TypeName(ws.Range("A156").Value) & "|" & ws.Range("A156").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A157").Formula = -0#
    ws.Range("B157").Value = TypeName(ws.Range("A157").Value) & "|" & ws.Range("A157").NumberFormat & "|" & Err.Number
    Err.Clear
    a(1, 1) = CByte(200): a(1, 2) = #1/2/2000 3:04:05 PM#: a(2, 1) = "'x": a(2, 2) = -0#
    ws.Range("C158:D158").Resize(2, 2).Value = a
    a(1, 1) = CInt(7): a(1, 2) = "1e3": a(2, 1) = "#N/A": a(2, 2) = -0#
    ws.Range("C159:D159").Resize(2, 2).Value = a
    a(1, 1) = "'x": a(1, 2) = Array(CCur(2.5), #1/1/2001#): a(2, 1) = 9.99999999999999E+307: a(2, 2) = CVErr(2042)
    ws.Range("C160:D160").Resize(2, 2).Value = a
End Sub
Public Sub FzMac32()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A161:B161").Value = Array(Empty, Null)
    ws.Range("E161").Value = Application.WorksheetFunction.Sum(ws.Range("A161:B161")) + Err.Number
    Err.Clear
    ws.Range("A162").Value = CLng(-5)
    ws.Range("B162").Value = TypeName(ws.Range("A162").Value) & "|" & ws.Range("A162").NumberFormat & "|" & Err.Number
    Err.Clear
    a(1, 1) = "1/2/2000": a(1, 2) = "#N/A": a(2, 1) = 9.99999999999999E+307: a(2, 2) = Array(1, "a", True)
    ws.Range("C163:D163").Resize(2, 2).Value = a
    ws.Range("A164").Value = Empty
    ws.Range("B164").Formula = "=A164+1"
    ws.Range("E164").Value = ws.Range("B164").Value + 0.5
    Err.Clear
    ws.Range("A165").Value = Array(1, "a", True)
    ws.Range("B165").Value = TypeName(ws.Range("A165").Value) & "|" & ws.Range("A165").NumberFormat & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac33()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    a(1, 1) = "#N/A": a(1, 2) = "  5 ": a(2, 1) = Empty: a(2, 2) = CVErr(2007)
    ws.Range("C166:D166").Resize(2, 2).Value = a
    a(1, 1) = CVErr(2042): a(1, 2) = CLng(-5): a(2, 1) = 1.5: a(2, 2) = CLng(-5)
    ws.Range("C167:D167").Resize(2, 2).Value = a
    ws.Range("A168:B168").Value = Array("x", CCur(1.5))
    ws.Range("E168").Value = Application.WorksheetFunction.Sum(ws.Range("A168:B168")) + Err.Number
    Err.Clear
    ws.Range("A169:B169").Value = Array("x", CCur(1.5))
    ws.Range("E169").Value = Application.WorksheetFunction.Sum(ws.Range("A169:B169")) + Err.Number
    Err.Clear
    ws.Range("A170:B170").Value = Array("x", CCur(1.5))
    ws.Range("E170").Value = Application.WorksheetFunction.Sum(ws.Range("A170:B170")) + Err.Number
    Err.Clear
End Sub
Public Sub FzMac34()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A171").Value = ""
    ws.Range("B171").Formula = "=A171+1"
    ws.Range("E171").Value = ws.Range("B171").Value + CCur(1)
    Err.Clear
    ws.Range("A172").Value2 = Empty
    ws.Range("B172").Value = TypeName(ws.Range("A172").Value) & "|" & ws.Range("A172").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A173").Value2 = CDec("1.2345678901234567890")
    ws.Range("B173").Value = TypeName(ws.Range("A173").Value) & "|" & ws.Range("A173").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Cells(174, 1).Value = 0.1 + 0.2
    v = ws.Cells(174, 1).Value: ws.Cells(174, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Cells(175, 1).Value = #6:00:00 AM#
    v = ws.Cells(175, 1).Value: ws.Cells(175, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac35()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A176:B176").Value = 5
    ws.Range("E176").Value = Application.WorksheetFunction.Sum(ws.Range("A176:B176")) + Err.Number
    Err.Clear
    ws.Range("A177").Value = True
    ws.Range("B177").Formula = "=A177&"""
    ws.Range("E177").Value = ws.Range("B177").Value + 1
    Err.Clear
    a(1, 1) = #1/2/2000 3:04:05 PM#: a(1, 2) = CByte(200): a(2, 1) = Array(CCur(2.5), #1/1/2001#): a(2, 2) = "1/2/2000"
    ws.Range("C178:D178").Resize(2, 2).Value = a
    ws.Range("A179").Value = CVErr(2042)
    ws.Range("B179").Formula = "=A179"
    ws.Range("E179").Value = ws.Range("B179").Value + CCur(1)
    Err.Clear
    ws.Cells(180, 1).Value = CCur(-0.005)
    v = ws.Cells(180, 1).Value: ws.Cells(180, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac36()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Cells(181, 1).Value = "1/2/2000"
    v = ws.Cells(181, 1).Value: ws.Cells(181, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    a(1, 1) = False: a(1, 2) = CDbl(CSng(1.1)): a(2, 1) = CInt(7): a(2, 2) = CCur(1.23456)
    ws.Range("C182:D182").Resize(2, 2).Value = a
    ws.Cells(183, 1).Value = CVErr(2007)
    v = ws.Cells(183, 1).Value: ws.Cells(183, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A184").Value2 = #1/2/2000 3:04:05 PM#
    ws.Range("B184").Value = TypeName(ws.Range("A184").Value) & "|" & ws.Range("A184").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Cells(185, 1).Value = ""
    v = ws.Cells(185, 1).Value: ws.Cells(185, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
End Sub
Public Sub FzMac37()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A186").Value = 1.5
    ws.Range("B186").Formula = "=A186*2"
    ws.Range("E186").Value = ws.Range("B186").Value + 0.5
    Err.Clear
    ws.Cells(187, 1).Value = 1.5
    v = ws.Cells(187, 1).Value: ws.Cells(187, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Cells(188, 1).Value = CCur(1.23456)
    v = ws.Cells(188, 1).Value: ws.Cells(188, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A189:B189").Value = Array(1, 2)
    ws.Range("E189").Value = Application.WorksheetFunction.Sum(ws.Range("A189:B189")) + Err.Number
    Err.Clear
    ws.Range("A190").Value = CInt(7)
    ws.Range("B190").Formula = "=A190&"""
    ws.Range("E190").Value = ws.Range("B190").Value + 1
    Err.Clear
End Sub
Public Sub FzMac38()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A191").Value = 0.1 + 0.2
    ws.Range("B191").Formula = "=A191=1"
    ws.Range("E191").Value = ws.Range("B191").Value + 0.5
    Err.Clear
    ws.Range("A192:B192").Value = Array(1, 2)
    ws.Range("E192").Value = Application.WorksheetFunction.Sum(ws.Range("A192:B192")) + Err.Number
    Err.Clear
    a(1, 1) = CSng(0.1): a(1, 2) = "=1+1": a(2, 1) = "1/2/2000": a(2, 2) = 0.1 + 0.2
    ws.Range("C193:D193").Resize(2, 2).Value = a
    ws.Range("A194").Formula = "TRUE"
    ws.Range("B194").Value = TypeName(ws.Range("A194").Value) & "|" & ws.Range("A194").NumberFormat & "|" & Err.Number
    Err.Clear
    ws.Range("A195").Value = "  5 "
    ws.Range("B195").Formula = "=A195=1"
    ws.Range("E195").Value = ws.Range("B195").Value + CCur(1)
    Err.Clear
End Sub
Public Sub FzMac39()
    On Error Resume Next
    Dim ws As Worksheet, v As Variant, a(1 To 2, 1 To 2) As Variant
    Set ws = Worksheets("Sheet2")
    ws.Range("A196:B196").Value = Array("x", CCur(1.5))
    ws.Range("E196").Value = Application.WorksheetFunction.Sum(ws.Range("A196:B196")) + Err.Number
    Err.Clear
    a(1, 1) = #1/2/2000 3:04:05 PM#: a(1, 2) = CCur(1.23456): a(2, 1) = CVErr(2007): a(2, 2) = CLng(-5)
    ws.Range("C197:D197").Resize(2, 2).Value = a
    ws.Range("A198:B198").Value = 5
    ws.Range("E198").Value = Application.WorksheetFunction.Sum(ws.Range("A198:B198")) + Err.Number
    Err.Clear
    ws.Cells(199, 1).Value = "#N/A"
    v = ws.Cells(199, 1).Value: ws.Cells(199, 2).Value = VarType(v) & "|" & CStr(v) & "|" & Err.Number
    Err.Clear
    ws.Range("A200:B200").Value = 5
    ws.Range("E200").Value = Application.WorksheetFunction.Sum(ws.Range("A200:B200")) + Err.Number
    Err.Clear
End Sub
