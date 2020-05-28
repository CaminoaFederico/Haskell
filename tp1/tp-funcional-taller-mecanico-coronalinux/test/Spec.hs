import PdePreludat
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Test de Patentes (punto 1)" $ do
    it "Costo de auto con patente AT001LN es 12500" $ do
      costoReparacion volkswagen `shouldBe` 12500
    it "Costo de auto con patente DJV214 es 18000" $ do
      costoReparacion ford `shouldBe` 18000
    it "Costo de auto con patente DJV215 es 20000" $ do
      costoReparacion renault `shouldBe` 20000
    it "Costo de auto con patente DFH029 es 15000" $ do
      costoReparacion chevrolet `shouldBe` 15000

  describe "Test auto peligroso (punto 2a)" $do
    it "Un auto cuya primera rueda tiene un desgaste menor o igual a 0.5" $do
      autoPeligroso chevrolet `shouldBe` False
    it "Un auto cuya primera rueda tiene un desgaste mayor a 0.5" $do
      autoPeligroso ford `shouldBe` True
 
  describe "Test revision auto (punto 2b)" $ do
    it "Un auto cuyo ultimo arreglo fue en 2016 no tiene que ser revisado" $ do
      revisionAuto volkswagen `shouldBe` False
    it "Un auto cuyo ultimo arreglo fue en 2015 tiene que ser revisado" $ do
      revisionAuto ford `shouldBe` True

  describe "Test de personal tecnico (punto 3a)" $ do
    it "Alfa hace que el auto regule a 2000 o lo deja como está" $ do
      ((<= 2000).rpm.operadorAlfa) ford `shouldBe` True  
    it "Bravo cambia todas las cubiertas dejandolas sin desgaste" $ do
      ((== [0, 0, 0, 0]).desgasteLlantas.operadorBravo) ford `shouldBe` True
    it "Charly hace lo mismo que alfa" $ do
      ((<= 2000).rpm.operadorCharly) ford `shouldBe` True
    it "Charly hace lo mismo que bravo" $ do
      ((== [0, 0, 0, 0]).desgasteLlantas.operadorCharly) ford `shouldBe` True



  describe "Test de personal tecnico (punto 3b)" $ do
    it "Tango no hace ningun arreglo sobre las llantas" $ do
      (desgasteLlantas.operadorTango) volkswagen `shouldBe` desgasteLlantas volkswagen 
    it "Tango no hace ningun arreglo sobre las rpm" $ do
      (rpm.operadorTango) volkswagen `shouldBe` rpm volkswagen
    it "Tango no hace ningun arreglo sobre la temperatura del agua" $ do
      (temperaturaAgua.operadorTango) volkswagen `shouldBe` temperaturaAgua volkswagen
    it "Zulu dejo la temperatura a 90" $ do
      (temperaturaAgua.operadorZulu) volkswagen `shouldBe` 90 
    it "Zulu hace lo que lima (cambiar las cubiertas delanteras dejandolas sin desgaste)" $ do
      (take 2.desgasteLlantas.operadorZulu) volkswagen `shouldBe` [0,0]
    it "Lima cambió las cubiertas delanteras dejandolas sin desgaste" $ do
      (take 2.desgasteLlantas.operadorLima) volkswagen `shouldBe` [0,0] 