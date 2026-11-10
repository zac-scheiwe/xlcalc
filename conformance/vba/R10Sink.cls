VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R10Sink"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Private WithEvents Src As R10Src
Public Id As String
Public Mode As Long

Public Sub Attach(ByVal s As R10Src)
    Set Src = s
End Sub

Public Sub Detach()
    Set Src = Nothing
End Sub

Private Sub Src_Ev1(ByVal n As Long, res As String)
    R10Log = R10Log & "[" & Id & ":" & n & "]"
    res = res & Id
    Select Case Mode
    Case 1: On Error Resume Next: Err.Raise 5: R10Log = R10Log & "(e" & Err.Number & ")"
    Case 2
        On Error Resume Next
        Err.Raise 9
        R10Log = R10Log & Err.Number
    Case 3
        On Error GoTo H
        Err.Raise 11
        R10Log = R10Log & "no"
        Exit Sub
H:
        R10Log = R10Log & "h" & Err.Number
        Resume Next
    Case 4: Src.Tag = "x"
    End Select
End Sub

Private Sub Src_Ev2()
    R10Log = R10Log & "{" & Id & "}"
    If Mode = 1 Then R10Log = R10Log & "(m1)"
End Sub
