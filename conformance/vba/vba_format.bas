Attribute VB_Name = "VbaFormat"
Option Explicit

Public SeenLog As String

Public Function VFmt(x As Variant, f As String) As String: VFmt = Format(x, f): End Function

Public Function Seen(r As Range) As Variant
    Dim v As Variant: v = r.Value2
    SeenLog = SeenLog & "[" & TypeName(v) & ":" & CStr(v) & "]"
    Seen = v
End Function
Public Function GetSeenLog() As String: GetSeenLog = SeenLog: SeenLog = "": End Function
