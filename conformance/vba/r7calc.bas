Attribute VB_Name = "R7Calc"
Option Explicit

Private Function Who() As String
    On Error Resume Next
    Who = Application.Caller.Parent.Name & "!" & Application.Caller.Address(False, False)
    If Err.Number <> 0 Then Who = "?"
End Function

Private Function Arg(ByVal x As Variant) As String
    On Error Resume Next
    If IsError(x) Then
        Arg = "#" & CStr(CLng(x))
    ElseIf IsObject(x) Then
        Arg = "obj"
    Else
        Arg = CStr(x)
    End If
    If Err.Number <> 0 Then Arg = "?"
End Function

Public Function UL(ByVal tag As String, ByVal x As Variant) As Variant
    EvLog.LogLine "U" & tag & "@" & Who() & "(" & Arg(x) & ")"
    UL = x
End Function

Public Function UV(ByVal tag As String, ByVal x As Variant) As Variant
    Application.Volatile True
    EvLog.LogLine "V" & tag & "@" & Who() & "(" & Arg(x) & ")"
    UV = x
End Function

Public Function DelSheet(ByVal n As String) As Long
    On Error GoTo EH
    Dim a As Boolean
    a = Application.DisplayAlerts
    Application.DisplayAlerts = False
    Worksheets(n).Delete
    Application.DisplayAlerts = a
    DelSheet = 0
    Exit Function
EH:
    DelSheet = Err.Number
End Function
