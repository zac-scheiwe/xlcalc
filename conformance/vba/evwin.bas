Attribute VB_Name = "EvW"
Option Explicit

Public LogS As String
Public H As EvWinApp

Public Function InitW() As Long
    On Error GoTo EH
    Set H = New EvWinApp
    Set H.App = Application
    InitW = 0
    Exit Function
EH:
    InitW = Err.Number
End Function

Public Sub Add(ByVal s As String)
    On Error GoTo EH
    LogS = LogS & s & "|"
    Exit Sub
EH:
    LogS = LogS & "Add error|"
End Sub

Public Function GetW() As String
    On Error GoTo EH
    Dim s As String
    Dim i As Long
    s = LogS & "#"
    For i = 1 To Application.Windows.Count
        s = s & " " & Application.Windows(i).Caption
    Next i
    s = s & " active=" & ActiveWorkbook.Name & " win=" & ActiveWindow.Caption
    LogS = ""
    GetW = s
    Exit Function
EH:
    GetW = "GetW error " & Err.Number
End Function
