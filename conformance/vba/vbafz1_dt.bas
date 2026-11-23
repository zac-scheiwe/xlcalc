Attribute VB_Name = "VbFz1Dt"
Public Function DtProbe(ByVal k As Long) As Variant
    Dim d As Variant, s As String
    On Error Resume Next
    Select Case k
    Case 0: d = Array(#1/2/2000#, #1/2/2000 3:04:05 PM#, #1/2/2000 10:04:05 PM#, #11/22/2000#, _
                      #11/22/2000 12:00:01 AM#, CDate(0.25), CDate(0.770833333333333), CDate(1.25), _
                      #3/1/1900#, #12/31/9999 11:59:59 PM#, CDate(36527.6278356482))
    Case 1: d = Array(#1/2/2000#, #1/2/2000 3:04:05 PM#, CDate(0.25))
    End Select
    Dim i As Long, v As Variant
    For i = LBound(d) To UBound(d)
        Err.Clear: v = Empty
        If k = 0 Then v = Application.Concat(d(i)) Else v = WorksheetFunction.Concat("<", d(i), ">")
        If Err.Number <> 0 Then s = s & "#E" & Err.Number & ";" Else s = s & "[" & v & "];"
    Next
    Err.Clear: v = Empty: v = Application.Len(#1/2/2000 3:04:05 PM#): s = s & "L" & v & ";"
    Err.Clear: v = Empty: v = Application.IsText(CDate(0.25)): s = s & "T" & v & ";"
    Err.Clear: v = Empty: v = Application.Days360(#1/2/2000#, #1/3/2000#): s = s & "Y" & v & ";"
    Err.Clear: v = Empty: v = Application.Days360("1/2/2000", "1/3/2000"): s = s & "YS" & v & ";"
    Err.Clear: v = Empty: v = Application.Text(#1/2/2000#, "dd mmm yyyy"): s = s & "X" & v & ";"
    Err.Clear: v = Empty: v = Application.Text("1/2/2000", "dd mmm yyyy"): s = s & "XS" & v & ";"
    Err.Clear: v = Empty: v = Application.Text("1/2/2000  3:04:05 PM", "dd mmm h:mm"): s = s & "XT" & v & ";"
    Err.Clear: v = Empty: v = Application.Sum("1/2/2000"): s = s & "S" & v & ";"
    DtProbe = s
End Function
