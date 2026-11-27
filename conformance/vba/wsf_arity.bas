Attribute VB_Name = "WsfArityMod"
Option Explicit
Private Function Dmp(v As Variant) As String
    If IsArray(v) Then
        Dmp = "Array"
    ElseIf VarType(v) = vbError Then
        Dmp = "Err:" & CLng(v)
    Else
        Dmp = TypeName(v) & ":" & CStr(v)
    End If
End Function
Public Function WsfArity(ByVal which As Long, ByVal viaWsf As Long) As String
    Dim o As Object, v As Variant
    If viaWsf = 0 Then
        Set o = Application
    Else
        Set o = Application.WorksheetFunction
    End If
    On Error Resume Next
    Select Case which
        Case 0: v = o.NumberValue("15")
        Case 1: v = o.NumberValue("1,5", ",")
        Case 2: v = o.USDollar(1.5)
        Case 3: v = o.TBillEq(40000, 40100)
        Case 4: v = o.TBillPrice(40000, 40100)
        Case 5: v = o.TBillYield(40000, 40100)
        Case 6: v = o.Xnpv(0.1, 1)
        Case 7: v = o.IsoWeekNum(40000, 1)
        Case 8: v = o.SumIfs(1, 2)
        Case 9: v = o.Pmt(0.1, 2)
        Case 10: v = o.NumberValue()
    End Select
    If Err.Number <> 0 Then
        WsfArity = Err.Number & "/" & Err.Description
        Err.Clear
    Else
        WsfArity = Dmp(v)
    End If
End Function
