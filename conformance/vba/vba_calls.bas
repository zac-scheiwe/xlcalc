Attribute VB_Name = "VbaCalls"
Option Explicit

Public Logs(0 To 5) As String

Public Function Seen(r As Range, tag As Long) As Variant
    Dim v As Variant: v = r.Value2
    Logs(tag) = Logs(tag) & "[" & TypeName(v) & ":" & CStr(v) & "]"
    Seen = v
End Function

Public Function GetSeenLog() As String
    Dim i As Long, s As String
    For i = 0 To 5
        s = s & Chr(65 + i) & Logs(i) & " "
        Logs(i) = ""
    Next i
    GetSeenLog = s
End Function
