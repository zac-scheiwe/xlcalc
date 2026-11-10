Attribute VB_Name = "VxUdf"
Option Explicit

Public Function Ux_1(ByVal d As Variant) As Variant
    On Error Resume Next
    Dim x As Variant
    x = 10 / d
    Ux_1 = "x=" & x & " e=" & Err.Number
End Function

Public Function Ux_2(ByVal k As Long) As Variant
    Select Case k
    Case 0: Ux_2 = CVErr(xlErrNull)
    Case 1: Ux_2 = CVErr(xlErrDiv0)
    Case 2: Ux_2 = CVErr(xlErrValue)
    Case 3: Ux_2 = CVErr(xlErrRef)
    Case 4: Ux_2 = CVErr(xlErrName)
    Case 5: Ux_2 = CVErr(xlErrNum)
    Case 6: Ux_2 = CVErr(xlErrNA)
    Case 7: Ux_2 = CVErr(2015)
    Case 8: Ux_2 = CVErr(13)
    Case 9: Ux_2 = CVErr(2042 + 1)
    Case 10: Ux_2 = CVErr(0)
    Case 11: Ux_2 = CVErr(2148)
    End Select
End Function

Public Function Ux_3(ByVal k As Long) As Variant
    Dim a(1 To 2, 1 To 3) As Variant, r(1 To 3) As Variant, i As Long, j As Long
    Select Case k
    Case 0
        For i = 1 To 2: For j = 1 To 3: a(i, j) = i * 10 + j: Next: Next
        Ux_3 = a
    Case 1
        a(1, 1) = 1: a(1, 2) = "t": a(1, 3) = CVErr(xlErrNA)
        a(2, 1) = True: a(2, 2) = Empty: a(2, 3) = Null
        Ux_3 = a
    Case 2
        Ux_3 = Split("a b c")
    Case 3
        Ux_3 = Array(Array(1, 2), Array(3, 4))
    Case 4
        Ux_3 = Array(1, "x", Empty, CVErr(xlErrDiv0), #1/1/2020#, 2.5, True)
    Case 5
        Dim m(0 To 1, 0 To 1) As Double
        m(0, 0) = 1.5: m(1, 1) = 2.5
        Ux_3 = m
    Case 6
        Dim e() As Variant
        Ux_3 = e
    Case 7
        Ux_3 = Array()
    Case 8
        Dim s(1 To 2) As String
        s(1) = "p": s(2) = "q"
        Ux_3 = s
    Case 9
        Dim l(1 To 2, 1 To 2) As Long
        l(1, 1) = 1: l(2, 2) = 4
        Ux_3 = l
    Case 10
        Ux_3 = Array(Array(1, 2), 3)
    Case 11
        r(1) = 5
        Ux_3 = r
    End Select
End Function

Private Sub Boom()
    Err.Raise 1234
End Sub

Public Function Ux_4() As Variant
    Boom
    Ux_4 = "no"
End Function

Public Function Ux_5(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: Ux_5 = 1 / 0
    Case 1: Ux_5 = CLng("x")
    Case 2: Err.Raise 5
    Case 3: Err.Raise 1000
    Case 4: Err.Raise vbObjectError + 7
    Case Else: Dim a(1 To 2) As Long: Ux_5 = a(5)
    End Select
    Exit Function
EH:
    Ux_5 = CVErr(Err.Number)
End Function

Public Function Ux_6() As Variant
    Debug.Print "u6"
    DoEvents
    Ux_6 = TypeName(Application.Caller) & ":" & Application.Caller.Address(0, 0)
End Function

Public Function Ux_7(ByVal k As Long) As Variant
    Dim o As Object
    Select Case k
    Case 0: Set Ux_7 = Nothing
    Case 1: Set Ux_7 = New VxLife
    Case 2: Set Ux_7 = Range("A1")
    Case 3: Ux_7 = Empty
    Case 4: Ux_7 = Null
    Case 5: Set Ux_7 = ThisWorkbook.Worksheets("Sheet1")
    Case 6: Ux_7 = New VxProp
    Case 7: Ux_7 = Err
    End Select
End Function

Public Function Ux_8(ByVal x As Long) As Long
    Ux_8 = OpmF(x)
End Function

Public Function Ux_9(ByVal x As Variant) As Variant
    On Error Resume Next
    Dim n As Long
    n = x + 1
    n = n * 2
    Ux_9 = n
    Ux_9 = Ux_9 & ":" & Err.Number
End Function

Public Function Ux_10() As Variant
    Dim a(1 To 1) As Double
    On Error Resume Next
    a(2) = 1
    a(1) = 5
    Ux_10 = a(1) & "/" & Err.Number
End Function

Public Function Ux_11(ByVal k As Long) As Variant
    Dim r As Double
    Rnd -1
    Randomize k
    r = Rnd
    Ux_11 = r & "/" & Rnd
End Function

Public Function Ux_12() As Variant
    Dim x As Long
    x = 7
    On Error GoTo 0
    On Error Resume Next
    x = Err.Number
    Dim y As Long
    y = 1 / 0
    Ux_12 = "x=" & x & " y=" & y & " e=" & Err.Number
End Function

Public Function Ux_13() As Variant
    Boom2
    Ux_13 = "after " & Err.Number
End Function

Private Sub Boom2()
    On Error Resume Next
    Err.Raise 5
End Sub
