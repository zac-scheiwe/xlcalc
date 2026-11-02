Attribute VB_Name = "VbaBoundary"
Option Explicit

Public CallCount As Long

Public Function SUM(x) As Double: SUM = 42: End Function
Public Function MyF(x) As Double: MyF = x * 100: End Function
Public Function MyG(x) As Double: MyG = x * 100: End Function

Public Function ReadA1() As Variant
    CallCount = CallCount + 1
    ReadA1 = Application.Caller.Worksheet.Range("A1").Value2
End Function
Public Function ReadA1Vol() As Variant
    Application.Volatile
    ReadA1Vol = Application.Caller.Worksheet.Range("A1").Value2
End Function
Public Function GetCallCount() As Long: GetCallCount = CallCount: End Function

Public Function DivZero() As Variant: DivZero = 1 / 0: End Function
Public Function RetNA() As Variant: RetNA = CVErr(xlErrNA): End Function
Public Function RetErrVal() As Variant: RetErrVal = CVErr(xlErrValue): End Function
Public Function RetArr() As Variant: RetArr = Array(1, 2, 3): End Function
Public Function ArgType(v As Variant) As String: ArgType = TypeName(v): End Function
Public Function TryWrite() As Variant
    On Error Resume Next
    Application.Caller.Worksheet.Range("Z1").Value2 = 99
    TryWrite = "err " & Err.Number
End Function

Public Function RetLong() As Long: RetLong = 123456: End Function
Public Function RetInteger() As Integer: RetInteger = -7: End Function
Public Function RetByte() As Byte: RetByte = 200: End Function
Public Function RetSingle() As Single: RetSingle = 0.1: End Function
Public Function RetDouble() As Double: RetDouble = 0.1: End Function
Public Function RetCurrency() As Currency: RetCurrency = 1.23456789: End Function
Public Function RetDate() As Date: RetDate = DateSerial(2020, 1, 2) + TimeSerial(3, 4, 5): End Function
Public Function RetDecimal() As Variant: RetDecimal = CDec("1.23456789012345678901"): End Function
Public Function RetBool() As Boolean: RetBool = True: End Function
Public Function RetString() As String: RetString = "abc": End Function
Public Function RetEmpty() As Variant: End Function
Public Function RetNull() As Variant: RetNull = Null: End Function
Public Function RetArr2() As Variant: RetArr2 = Array(1, "a", 2.5): End Function
Public Function RetMatrix() As Variant
    Dim m(1 To 2, 1 To 2) As Double
    m(1, 1) = 1: m(1, 2) = 2: m(2, 1) = 3: m(2, 2) = 4
    RetMatrix = m
End Function
Public Function RunDivZero() As Double: RunDivZero = 1 / 0: End Function
Public Function RunRaise(n As Long) As Long: Err.Raise n: End Function
Public Function RunTypeName(v As Variant) As String: RunTypeName = TypeName(v): End Function
Public Function RunEcho(v As Variant) As Variant: RunEcho = v: End Function
Public Function RunAdd(a As Variant, b As Variant) As Variant: RunAdd = a + b: End Function

Public Function WfMatchErr() As String
    Dim r As Variant
    On Error Resume Next
    r = Application.WorksheetFunction.Match(999, Array(1, 2, 3), 0)
    WfMatchErr = "err " & Err.Number
End Function
Public Function AppMatch() As String
    Dim r As Variant
    On Error Resume Next
    r = Application.Match(999, Array(1, 2, 3), 0)
    AppMatch = TypeName(r) & "/" & IIf(IsError(r), CLng(r), 0) & " errNo:" & Err.Number
End Function

Public Function CatchDivZero() As Variant
    Dim r As Variant
    On Error Resume Next
    r = DivZero()
    CatchDivZero = Array(Err.Number, Err.Description)
End Function
Public Function CatchRunDivZero() As Variant
    Dim r As Double
    On Error Resume Next
    r = RunDivZero()
    CatchRunDivZero = Array(Err.Number, Err.Description)
End Function
Public Function CatchRaise(n As Long) As Variant
    Dim r As Long
    On Error Resume Next
    r = RunRaise(n)
    CatchRaise = Array(Err.Number, Err.Description)
End Function
