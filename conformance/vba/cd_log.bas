Attribute VB_Name = "CdLog"
Option Explicit

Private Function Show(ByVal e As Variant) As String
    If IsError(e) Then
        Show = "#err"
    ElseIf IsEmpty(e) Then
        Show = "E"
    Else
        Show = CStr(e)
    End If
End Function

Public Function CdR(ByVal tag As String, x As Variant) As Variant
    On Error GoTo EH
    Dim v As Variant, e As Variant, s As String
    If TypeName(x) = "Range" Then v = x.Value2 Else v = x
    If IsArray(v) Then
        For Each e In v
            s = s & Show(e) & ";"
        Next
    Else
        s = Show(v)
    End If
    CsLog.LogLine "R " & tag & " " & s
    CdR = 0
    Exit Function
EH:
    CsLog.LogLine "CdR error " & Err.Number
    CdR = -1
End Function
