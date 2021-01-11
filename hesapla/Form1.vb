Public Class Form1
    Private Sub cik_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cik.Click
        End
    End Sub
    Private Sub hesapla_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles hesapla.Click
        'Dönüştürme
        Dim sonuc As Decimal
        For Each a In Me.Controls
            If TypeOf a Is TextBox And a.tabindex < 23 Then
                If a.text = "" Then
                    a.text = "0"
                End If
                sonuc = Convert.ToDecimal(a.text)
                a.Text = sonuc.ToString("#,##0.00")
            End If
        Next
        Dim davadeger As Decimal
        Dim davakabul As Decimal
        Dim davared As Decimal
        Dim tharc As Decimal
        Dim pharc As Decimal
        Dim iharc As Decimal
        Dim bharc As Decimal
        Dim fharc As Decimal
        Dim vekildavaci As Decimal
        Dim vekildavali As Decimal
        Dim dctopmas As Decimal = 0
        Dim dltopmas As Decimal = 0
        '---------------------------------ÇEVİRİLER---------------------------------
        If dadeger.Text <> "" Then
            davadeger = Convert.ToDecimal(dadeger.Text)
        Else
            davadeger = 0
        End If
        If dakabul.Text <> "" Then
            davakabul = Convert.ToDecimal(dakabul.Text)
        Else
            davakabul = 0
        End If
        davared = davadeger - davakabul
        tharc = (davakabul / 1000) * 68.31
        dared.Text = davared
        If Val(pesinharc.Text) > 0 Then
            pharc = Convert.ToDecimal(pesinharc.Text)
        End If
        If Val(islahharci.Text) > 0 Then
            iharc = Convert.ToDecimal(islahharci.Text)
        End If
        '----------------------------------HARÇLAR----------------------------------
        If davakabul = 0 Then
            tharc = 59.3
            toplamharc.Text = Math.Round(tharc, 2)
            If pharc + iharc > tharc Then
                fazlaharc.Text = Math.Round((pharc + iharc) - tharc, 2)
                fharc = Convert.ToDecimal(fazlaharc.Text)
            ElseIf pharc + iharc < tharc Then
                bakiyeharc.Text = Math.Round(tharc - (pharc + iharc), 2)
                bharc = Convert.ToDecimal(bakiyeharc.Text)
            End If
        End If
        If davakabul > 0 Then
            If tharc < 59.3 Then
                tharc = 59.3
            End If
            toplamharc.Text = Math.Round(tharc, 2)
            If pharc + iharc > tharc Then
                fazlaharc.Text = Math.Round((pharc + iharc) - tharc, 2)
                fharc = Convert.ToDecimal(fazlaharc.Text)
            ElseIf pharc + iharc < tharc Then
                bakiyeharc.Text = Math.Round(tharc - (pharc + iharc), 2)
                bharc = Convert.ToDecimal(bakiyeharc.Text)
            End If
        End If
        tharc = Math.Round(tharc, 2)
        '-----------------------------VEKALET ÜCRETLERİ-----------------------------
        If davakabul <= 4080 Then
            vekildavaci = davakabul
        End If
        If davared <= 4080 Then
            vekildavali = davared
        End If
        If davakabul > 4080 And davakabul <= 27200 Then
            vekildavaci = 4080
        End If
        If davared > 4080 And davared <= 27200 Then
            vekildavali = 4080
        End If
        If davakabul > 27200 And davakabul <= 40000 Then
            vekildavaci = davakabul * 0.15
        End If
        If davared > 27200 And davared <= 40000 Then
            vekildavali = davared * 0.15
        End If
        If davakabul > 40000 And davakabul <= 90000 Then
            vekildavaci = ((davakabul - 40000) * 0.13) + 6000
        End If
        If davared > 40000 And davared <= 90000 Then
            vekildavali = ((davared - 40000) * 0.13) + 6000
        End If
        If davakabul > 90000 And davakabul <= 180000 Then
            vekildavaci = ((davakabul - 90000) * 0.095) + 12500
        End If
        If davared > 90000 And davared <= 180000 Then
            vekildavali = ((davared - 90000) * 0.095) + 12500
        End If
        If davakabul > 180000 And davakabul <= 430000 Then
            vekildavaci = ((davakabul - 180000) * 0.07) + 21050
        End If
        If davared > 180000 And davared <= 430000 Then
            vekildavali = ((davared - 180000) * 0.07) + 21050
        End If
        If davakabul > 430000 And davakabul <= 1050000 Then
            vekildavaci = ((davakabul - 430000) * 0.05) + 38550
        End If
        If davared > 430000 And davared <= 1050000 Then
            vekildavali = ((davared - 430000) * 0.05) + 38550
        End If
        If davakabul > 1050000 And davakabul <= 1825000 Then
            vekildavaci = ((davakabul - 1050000) * 0.035) + 69550
        End If
        If davared > 1050000 And davared <= 1825000 Then
            vekildavali = ((davared - 1050000) * 0.035) + 69550
        End If
        If davakabul > 1825000 And davakabul <= 3100000 Then
            vekildavaci = ((davakabul - 1825000) * 0.018) + 96675
        End If
        If davared > 1825000 And davared <= 3100000 Then
            vekildavali = ((davared - 1825000) * 0.018) + 96675
        End If
        If davakabul > 3100000 Then
            vekildavaci = ((davakabul - 3100000) * 0.01) + 119625
        End If
        If davared > 3100000 Then
            vekildavali = ((davared - 3100000) * 0.01) + 119625
        End If
        dcvekiliucret.Text = Math.Round(vekildavaci, 2)
        dlvekiliucret.Text = Math.Round(vekildavali, 2)
        '-----------------------------------YAZIM-----------------------------------
        '-----------------------------------HARÇ------------------------------------       
        If tharc > pharc + iharc And davakabul > 0 Then
            If pharc = 0 And iharc = 0 Then
                yazimharc.Text = "Alınması gereken " + tharc.ToString("#,##0.00") + "-TL karar ve ilam harcının davalıdan alınarak hazineye irat kaydına,"
            End If
            If pharc > 0 And iharc > 0 Then
                yazimharc.Text = "Alınması gereken " + tharc.ToString("#,##0.00") + "-TL karar ve ilam harcından, peşin yatırılan " + pharc.ToString("#,##0.00") + "-TL harcın ve " + iharc.ToString("#,##0.00") + "-TL tamamlama / ıslah harcının mahsubu ile bakiye kalan " + bharc.ToString("#,##0.00") + "-TL harcın davalıdan alınarak hazineye irat kaydına,"
            End If
            If pharc > 0 And iharc = 0 Then
                yazimharc.Text = "Alınması gereken " + tharc.ToString("#,##0.00") + "-TL karar ve ilam harcının, peşin yatırılan " + pharc.ToString("#,##0.00") + "-TL harçtan mahsubu ile bakiye kalan " + bharc.ToString("#,##0.00") + "-TL harcın davalıdan alınarak hazineye irat kaydına,"
            End If
            If pharc = 0 And iharc > 0 Then
                yazimharc.Text = "Alınması gereken " + tharc.ToString("#,##0.00") + "-TL karar ve ilam harcının, " + iharc.ToString("#,##0.00") + "-TL ıslah / tamamlama harcından mahsubu ile bakiye kalan " + bharc.ToString("#,##0.00") + "-TL harcın davalıdan alınarak hazineye irat kaydına,"
            End If
        End If
        If tharc < pharc + iharc And davakabul > 0 Then
            If pharc > 0 And iharc > 0 Then
                yazimharc.Text = "Alınması gereken " + tharc.ToString("#,##0.00") + "-TL karar ve ilam harcının, peşin yatırılan " + pharc.ToString("#,##0.00") + "-TL harçtan ve " + iharc.ToString("#,##0.00") + "-TL ıslah / tamamlama harcından mahsubu ile fazla yatırılan " + fharc.ToString("#,##0.00") + "-TL harcın karar kesinleştiğinde ve talep halinde davacıya iadesine,"
            End If
            If pharc > 0 And iharc = 0 Then
                yazimharc.Text = "Alınması gereken " + tharc.ToString("#,##0.00") + "-TL karar ve ilam harcının, peşin yatırılan " + pharc.ToString("#,##0.00") + "-TL harçtan mahsubu ile fazla yatırılan " + fharc.ToString("#,##0.00") + "-TL harcın karar kesinleştiğinde ve talep halinde davacıya iadesine,"
            End If
            If pharc = 0 And iharc > 0 Then
                yazimharc.Text = "Alınması gereken " + tharc.ToString("#,##0.00") + "-TL karar ve ilam harcının " + iharc.ToString("#,##0.00") + "-TL ıslah / tamamlama harcından mahsubu ile fazla yatırılan " + fharc.ToString("#,##0.00") + "-TL harcın karar kesinleştiğinde ve talep halinde davacıya iadesine,"
            End If
        End If
        If davakabul = 0 Then
            If pharc > 0 And iharc > 0 Then
                yazimharc.Text = "Alınması gereken 59,30-TL red harcının peşin yatırılan " + pharc.ToString("#,##0.00") + "-TL harçtan ve " + iharc.ToString("#,##0.00") + "-TL ıslah / tamamlama harcından mahsubu ile fazla yatırılan " + fharc.ToString("#,##0.00") + "-TL harcın karar kesinleştiğinde ve talep halinde davacıya iadesine,"
            End If
            If pharc < 59.3 And iharc = 0 Then
                yazimharc.Text = "Alınması gereken 59,30-TL red harcının, peşin yatırılan " + pharc.ToString("#,##0.00") + "-TL harçtan mahsubu ile bakiye " + bharc.ToString("#,##0.00") + "-TL harcın davacıdan alınarak hazineye irat kaydına,"
            End If
            If pharc >= 59.3 And iharc = 0 Then
                yazimharc.Text = "Alınması gereken 59,30-TL red harcının peşin yatırılan " + pharc.ToString("#,##0.00") + "-TL'sı harçtan mahsubu ile fazla yatırılan " + fharc.ToString("#,##0.00") + "-TL'sı harcın karar kesinleştiğinde ve talep halinde davacıya iadesine,"
            End If
            If pharc = 0 And iharc >= 59.3 Then
                yazimharc.Text = "Tahakkuk eden 59,30-TL red harcının peşin yatırılan " + iharc.ToString("#,##0.00") + "-TL ıslah / tamamlama harcından mahsubu ile fazla yatırılan " + fharc.ToString("#,##0.00") + "-TL harcın karar kesinleştiğinde ve talep halinde davacıya iadesine,"
            End If
            If pharc = 0 And iharc = 0 Then
                yazimharc.Text = "Tahakkuk eden 59,30-TL red harcının davacıdan alınarak hazineye irat kaydına,"
            End If
        End If
        If tharc = pharc + iharc Then
            yazimharc.Text = "Peşin alınan harcın mahsubu ile başkaca harç alınmasına yer olmadığına,"
        End If
        '-----------------------------------AAÜT------------------------------------
        If davakabul > 0 And davakabul < 4080 Then
            yazimdcvekili.Text = "Davacı davada kendini bir vekil ile temsil ettirdiğinden, karar tarihinde yürürlükte bulunan AAÜT'nin 13/2 maddesine göre hesaplanan " + Math.Round(vekildavaci, 2).ToString("#,##0.00") + "-TL vekalet ücretinin davalıdan alınarak, davacıya verilmesine,"
        End If
        If davared > 0 And davared < 4080 Then
            yazimdlvekili.Text = "Davalı davada kendini bir vekil ile temsil ettirdiğinden, karar tarihinde yürürlükte bulunan AAÜT'nin 13/2 maddesine göre hesaplanan " + Math.Round(vekildavali, 2).ToString("#,##0.00") + "-TL vekalet ücretinin davacıdan alınarak, davalıya verilmesine,"
        End If
        If davakabul >= 4080 And davakabul <= 27200 Then
            yazimdcvekili.Text = "Davacı davada kendini bir vekil ile temsil ettirdiğinden, karar tarihinde yürürlükte bulunan AAÜT'nin 13/1 maddesine göre hesaplanan 4.080,00-TL'sı maktu vekalet ücretinin davalıdan alınarak, davacıya verilmesine,"
        End If
        If davared >= 4080 And davared <= 27200 Then
            yazimdlvekili.Text = "Davalı kendisini vekili vasıtasıyla temsil ettirdiğinden karar tarihinde yürürlükte bulunan AAÜT'nin 13/1 maddesine göre hesaplanan 4.080,00-TL'sı maktu vekalet ücretinin davacıdan alınarak, davalıya verilmesine,"
        End If
        If davakabul > 27200 Then
            yazimdcvekili.Text = "Davacı davada kendini bir vekil ile temsil ettirdiğinden, karar tarihinde yürürlükte bulunan AAÜT'sine göre hesaplanan " + Math.Round(vekildavaci, 2).ToString("#,##0.00") + "-TL nispi vekalet ücretinin davalıdan alınarak, davacıya verilmesine,"
        End If
        If davared > 27200 Then
            yazimdlvekili.Text = "Davalı davada kendini bir vekil ile temsil ettirdiğinden, karar tarihinde yürürlükte bulunan AAÜT'sine göre hesaplanan " + Math.Round(vekildavali, 2).ToString("#,##0.00") + "-TL nispi vekalet ücretinin davacıdan alınarak, davalıya verilmesine,"
        End If
        '-----------------------------------ORAN------------------------------------
        If davakabul > 0 And davakabul <= davadeger Then
            kabulorani.Text = Math.Round((davakabul / davadeger) * 100, 2)
            redorani.Text = Math.Round((davared / davadeger) * 100, 2)
        Else
            kabulorani.Text = "0"
            redorani.Text = "100"
        End If

        '---------------------------------MASRAFLAR---------------------------------
        'If davakabul > 0 Then
        'If (pharc > 0 Or iharc > 0) And pharc + iharc >= tharc Then
        'masrafharc.Text = "Davacı tarafından yatırılan ve karar ve ilam harcı olarak mahsup edilen " + tharc.toString("#,##0.00") + "-TL'sı harcın davalıdan alınarak davacıya verilmesine,"
        'End If
        'If (pharc > 0 Or iharc > 0) And pharc + iharc < tharc Then
        'masrafharc.Text = "Davacı tarafından yatırılan ve karar ve ilam harcı olarak mahsup edilen " + (pharc + iharc).toString("#,##0.00") + "-TL'sı harcın davalıdan alınarak davacıya verilmesine,"
        'End If
        'End If
        masrafdavaci.Text = "Davacı tarafından yapılan dosyada sarf ve evrakı mevcut "
        masrafdavali.Text = "Davalı tarafından yapılan dosyada sarf ve evrakı mevcut "
        If davakabul > 0 Then
            If (pharc > 0 Or iharc > 0) And pharc + iharc >= tharc Then
                masrafdavaci.Text = masrafdavaci.Text + tharc.ToString("#,##0.00") + " TL harç gideri, "
                dctopmas = dctopmas + tharc
            End If
            If (pharc > 0 Or iharc > 0) And pharc + iharc < tharc Then
                masrafdavaci.Text = masrafdavaci.Text + (pharc + iharc).ToString("#,##0.00") + " TL harç gideri, "
                dctopmas = dctopmas + (pharc + iharc)
            End If
        End If
        If Val(dckesifharc.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dckesifharc.Text + " TL keşif harcı gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dckesifharc.Text)
        End If
        If Val(dlkesifharci.Text) > 0 Then
            masrafdavali.Text = masrafdavali.Text + dlkesifharci.Text + " TL keşif harcı gideri, "
            dltopmas = dltopmas + Convert.ToDecimal(dlkesifharci.Text)
        End If
        If Val(dcbilirkisi.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dcbilirkisi.Text + " TL bilirkişi gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dcbilirkisi.Text)
        End If
        If Val(dlbilirkisi.Text) > 0 Then
            masrafdavali.Text = masrafdavali.Text + dlbilirkisi.Text + " TL bilirkişi gideri, "
            dltopmas = dltopmas + Convert.ToDecimal(dlbilirkisi.Text)
        End If
        If Val(dctaksi.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dctaksi.Text + " TL taksi gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dctaksi.Text)
        End If
        If Val(dltaksi.Text) > 0 Then
            masrafdavali.Text = masrafdavali.Text + dltaksi.Text + " TL taksi gideri, "
            dltopmas = dltopmas + Convert.ToDecimal(dltaksi.Text)
        End If
        If Val(dctebligat.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dctebligat.Text + " TL tebligat gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dctebligat.Text)
        End If
        If Val(dltebligat.Text) > 0 Then
            masrafdavali.Text = masrafdavali.Text + dltebligat.Text + " TL tebligat gideri, "
            dltopmas = dltopmas + Convert.ToDecimal(dltebligat.Text)
        End If
        If Val(dcposta.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dcposta.Text + " TL posta gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dcposta.Text)
        End If
        If Val(dlposta.Text) > 0 Then
            masrafdavali.Text = masrafdavali.Text + dlposta.Text + " TL posta gideri, "
            dltopmas = dltopmas + Convert.ToDecimal(dlposta.Text)
        End If
        If Val(dcadlitip.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dcadlitip.Text + " TL adli tıp gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dcadlitip.Text)
        End If
        If Val(dladlitip.Text) > 0 Then
            masrafdavali.Text = masrafdavali.Text + dladlitip.Text + " TL adli tıp gideri, "
            dltopmas = dltopmas + Convert.ToDecimal(dladlitip.Text)
        End If
        If Val(dctanik.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dctanik.Text + " TL tanık gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dctanik.Text)
        End If
        If Val(dltanik.Text) > 0 Then
            masrafdavali.Text = masrafdavali.Text + dltanik.Text + " TL tanık gideri, "
            dltopmas = dltopmas + Convert.ToDecimal(dltanik.Text)
        End If
        If Val(dcfotokopi.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dcfotokopi.Text + " TL talimat gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dcfotokopi.Text)
        End If
        If Val(dlfotokopi.Text) > 0 Then
            masrafdavali.Text = masrafdavali.Text + dlfotokopi.Text + " TL talimat gideri, "
            dltopmas = dltopmas + Convert.ToDecimal(dlfotokopi.Text)
        End If
        If Val(dctespit.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dctespit.Text + " TL tespit gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dctespit.Text)
        End If
        If Val(dcnoter.Text) > 0 Then
            masrafdavaci.Text = masrafdavaci.Text + dcnoter.Text + " TL noter gideri, "
            dctopmas = dctopmas + Convert.ToDecimal(dcnoter.Text)
        End If
        If davakabul = 0 And dctopmas > 0 Then
            masrafdavaci.Text = "Davacının yaptığı masrafların üzerinde bırakılmasına,"
        End If
        If davared = 0 And dltopmas > 0 Then
            masrafdavali.Text = "Davalının yaptığı masrafların üzerinde bırakılmasına,"
        End If
        If davakabul > 0 And Val(kabulorani.Text) < 100 Then
            masrafdavaci.Text = masrafdavaci.Text + "olmak üzere toplam " + dctopmas.ToString("#,##0.00") + " TL yargılama masrafının davanın %" + kabulorani.Text + " kabul oranına göre hesaplanan " + Math.Round(((dctopmas / 100) * (Convert.ToDecimal(kabulorani.Text))), 2).ToString("#,##0.00") + "-TL'sının davalıdan alınarak davacıya verilmesine,"
        End If
        If davared > 0 And Val(redorani.Text) < 100 Then
            masrafdavali.Text = masrafdavali.Text + "olmak üzere toplam " + dltopmas.ToString("#,##0.00") + " TL yargılama masrafının davanın %" + redorani.Text + " red oranına göre hesaplanan " + Math.Round(((dltopmas / 100) * (Convert.ToDecimal(redorani.Text))), 2).ToString("#,##0.00") + "-TL'sının davacıdan alınarak davalıya verilmesine,"
        End If
        If davakabul = davadeger Then
            masrafdavaci.Text = masrafdavaci.Text + "olmak üzere toplam " + dctopmas.ToString("#,##0.00") + " TL yargılama masrafının davalıdan alınarak davacıya verilmesine,"
        End If
        If davared = davadeger Then
            masrafdavali.Text = masrafdavali.Text + "olmak üzere toplam " + dltopmas.ToString("#,##0.00") + " TL yargılama masrafının davacıdan alınarak davalıya verilmesine,"
        End If
        If dctopmas = 0 Then
            masrafdavaci.Text = ""
        End If
        If dltopmas = 0 Then
            masrafdavali.Text = ""
        End If
        iade1.Text = "Dosya kapsamında bulunan bakiye gider avansının, HMK'nun 333. maddesi ve Hukuk Muhakemeleri Kanunu Yönetmeliği'nin 47/1 maddesi uyarınca, kararın kesinleşmesine müteakip davacı tarafa iadesine,"
        iade2.Text = "Dosya kapsamında bulunan bakiye gider avansının, HMK'nun 333. maddesi ve Hukuk Muhakemeleri Kanunu Yönetmeliği'nin 47/1 maddesi uyarınca, kararın kesinleşmesine müteakip davalı tarafa iadesine,"
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        ClearTextBox(Me)
    End Sub
    Public Sub ClearTextBox(ByVal root As Control)
        For Each ctrl As Control In root.Controls
            ClearTextBox(ctrl)
            If TypeOf ctrl Is TextBox Then
                CType(ctrl, TextBox).Text = String.Empty
            End If
        Next ctrl
    End Sub
End Class
