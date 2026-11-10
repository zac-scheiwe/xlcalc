Attribute VB_Name = "R10Mod"
Option Explicit

Public R10Log As String
Public R10Mode As Long
Public R10Depth As Long

Public Sub R10Reset()
    On Error Resume Next
    Application.EnableEvents = True
    Application.ScreenUpdating = True
    Application.Calculation = xlCalculationAutomatic
    R10Mode = 0
    R10Depth = 0
    Err.Clear
End Sub

Public Function R10Take() As String
    R10Take = R10Log
    R10Log = ""
End Function

Public Sub R10Hook(ByVal tag As String, ByVal Target As Range)
    Dim c As Range
    Select Case R10Mode
    Case 1
        Application.EnableEvents = False
        Target.Cells(1, 1).Offset(0, 5).Value = 1
        Application.EnableEvents = True
        R10Log = R10Log & "(m1)"
    Case 2
        If R10Depth < 3 Then
            R10Depth = R10Depth + 1
            Target.Cells(1, 1).Offset(1, 0).Value = R10Depth
            R10Depth = R10Depth - 1
        End If
    Case 3
        R10Log = R10Log & "(calc=" & Application.Calculation & ")"
        Application.Calculation = xlCalculationManual
    Case 4
        R10Log = R10Log & "(su=" & Application.ScreenUpdating & ")"
        Application.ScreenUpdating = False
    Case 5
        On Error Resume Next: Err.Raise 5: R10Log = R10Log & "(e" & Err.Number & ")"
    Case 6
        R10Log = R10Log & "(ev=" & Application.EnableEvents & ")"
        Application.EnableEvents = False
    End Select
End Sub
