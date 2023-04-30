import javafx.beans.property.{BooleanProperty, DoublePropertyBase, ObjectProperty}

import scala.collection.mutable.Buffer
import javafx.event.ActionEvent
import org.w3c.dom.events.MouseEvent
import scalafx.scene.control.Alert.AlertType
import scalafx.application.*
import scalafx.beans.property.*
import scalafx.collections.ObservableBuffer
import scalafx.event
import scalafx.geometry.*
import scalafx.scene.*
import scalafx.geometry.Insets
import scalafx.geometry.Bounds
import scalafx.scene.effect.DropShadow
import scalafx.scene.paint.Color.*
import scalafx.scene.paint.*
import scalafx.scene.text.Text
import scalafx.scene.*
import scalafx.scene.layout.*
import scalafx.scene.control.*
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.ScatterChart
import scalafx.scene.chart.XYChart
import scalafx.scene.image.ImageView
import scalafx.stage.Stage

import java.io.File


object Dashboard extends JFXApp3 {

  var fighterData: FighterData = new FighterData

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage {
      title = "ufcdash"

      //Houses elements as the UIElement class to make accessing UIElement-specific methods and variables possible,
      // unlike in elements.children, which saves elements as scalafx.scene.Node
      val elementsBuffer: Buffer[UIElementParent] = Buffer()

      val APIfailAlert = new Alert(AlertType.Error) {
        initOwner(stage)
        title = "API call fail"
        headerText = "The application tried to initiate an API call, but failed."
        contentText = "Check your internet connection or API key."
      }

      val invalidNameAlert = new Alert(AlertType.Error) {
        initOwner(stage)
        title = "Invalid filename"
        headerText = "The inputted filename was invalid."
        contentText = "Try a different filename."
      }

      val xmlReadFail = new Alert(AlertType.Error) {
        initOwner(stage)
        title = "XML read fail"
        headerText = "The program tried to read the XML file, but failed."
        contentText = "The XML file may be corrupted."
      }

      //Dialog for the selection of a given fighter
      val fighterSelectDialog = new Dialog [Unit] () {
        initOwner(stage)
        title = "Fighter Select"
        headerText = "Select weight class and fighter"
        scalafx.Includes.jfxDialogPane2sfx(dialogPane()).buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
      }

      val weightClassSelect = new ComboBox(FileHandler.getRankedFighters.map(_._1))
      weightClassSelect.setPromptText("Weight Class")
      val fighterSelect = new ComboBox[String]()
      fighterSelect.setPromptText("Fighter")

      weightClassSelect.onAction = (e: javafx.event.ActionEvent) =>
        fighterSelect.items = scalafx.collections.ObservableBuffer[String]()
        FileHandler.getRankedFighters.find(_._1 == weightClassSelect.value.apply()).getOrElse(("", List[(String, String)](("",""))))._2.foreach((n: (String,String)) => fighterSelect.+=(n._1))

      val fighterSelectGrid = new GridPane() {
        hgap = 12
        vgap = 12
        padding = Insets(20, 100, 10, 10)

        add(weightClassSelect, 0, 0)
        add(fighterSelect, 0, 1)
      }

      scalafx.Includes.jfxDialogPane2sfx(fighterSelectDialog.dialogPane()).content = fighterSelectGrid

      //Selects fighter once user presses OK
      fighterSelectDialog.resultConverter = dialogButton =>
        if (dialogButton == ButtonType.OK) then
          val selectedFighter = fighterSelect.value.apply()

          if selectedFighter.nonEmpty then
            try
              val correctWeightClass = FileHandler.getRankedFighters.find(_._2.exists(_._1 == selectedFighter))
              if correctWeightClass.nonEmpty then
                val fighterNameId = correctWeightClass.get._2.find(_._1 == selectedFighter)
                fighterData = FileHandler.getFighter(fighterNameId.get._1, fighterNameId.get._2)
                defaultElement.update(fighterData)
                elementsBuffer.foreach(elem =>
                  elem.fighterData = fighterData;
                  elem.getChild.update;
                )
            catch
              case _ => APIfailAlert.showAndWait()

      val fileSelectDialog = new TextInputDialog() {
        initOwner(stage)
        title = "File Select"
        headerText = "XML File Select"
        contentText = "Please enter the XML file name (omit .xml):"
      }

      val fighterXMLSelectDialog = new TextInputDialog() {
        initOwner(stage)
        title = "File Select"
        headerText = "Enter fighter profile xml"
        contentText = "Ensure both the fighter profile and summary exist, with correct formatting:"
      }

      val menuBar = new MenuBar
      //The file menu handles saving and loading
      val file = new Menu("File")
      val saveItem = new MenuItem("Save Dashboard") {
        this.onAction = (e: ActionEvent) =>
          val result = fileSelectDialog.showAndWait()
          if result.nonEmpty && elementsBuffer.nonEmpty then
            try
              FileHandler.saveFile("saves\\" + result.get.trim + ".xml", DataParser.elementsToXML(elementsBuffer.toSeq))
            catch
              case _ =>
                invalidNameAlert.showAndWait()
            finally
              fileSelectDialog.editor.text = ""
      }
      val loadItem = new MenuItem("Load Dashboard") {
        this.onAction = (e: ActionEvent) =>
          val result = fileSelectDialog.showAndWait()
          if result.nonEmpty then
            try
              val elementsToAdd = DataParser.xmlToElements(xml.XML.loadFile("saves\\" + result.get.trim + ".xml"), elementControl)
              val initSize = elementsBuffer.size
              var x = 0
              while x < initSize do
                elementsBuffer.head.deleteDuplicate(elementsBuffer.head, true)
                x += 1
              elementsToAdd.foreach(_.fighterData = fighterData)
              elementsToAdd.foreach(_.getChild.update)
              elementsToAdd.foreach(elementsBuffer += _)
              elementsToAdd.foreach(elem => elements.children.add(elements.children.size - 1, elem))
            catch
              case e: java.io.FileNotFoundException => invalidNameAlert.showAndWait()
              case _ => xmlReadFail.showAndWait()
            finally
                fileSelectDialog.editor.text = ""
      }
      file.items = List(saveItem, loadItem)

      val fighters = new Menu("Fighters")
      val fighterSelectItem = new MenuItem("Select Fighter") {
        this.onAction = (e: javafx.event.ActionEvent) =>
          fighterSelectDialog.showAndWait()
      }
      val fromFileSelectItem = new MenuItem("Select Fighter From XML") {
        this.onAction = (e: javafx.event.ActionEvent) =>
          val result = fighterXMLSelectDialog.showAndWait()
          if result.nonEmpty then
            try
              fighterData = FileHandler.getFighterFromFile("FighterData\\" + result.get + ".xml")
              defaultElement.update(fighterData)
              elementsBuffer.foreach(elem =>
                elem.fighterData = fighterData;
                elem.getChild.update;
              )
            catch
              case e: java.io.FileNotFoundException =>
                invalidNameAlert.showAndWait()
              case _ =>
                xmlReadFail.showAndWait()
            finally
              fighterXMLSelectDialog.editor.text = ""
      }
      val fighterListItem = new MenuItem("Regenerate Fighter List")
      fighters.items = List(fighterSelectItem, fromFileSelectItem, fighterListItem)

      //Handles alert for regenerating fighter list
      val listRegenAlert = new Alert(Alert.AlertType.Confirmation) {
        initOwner(stage)
        title = "Warning"
        headerText = "Are you sure you want to proceed?"
        contentText = "Regenerating the fighter list will produce a new API call.\nNote: This may take a while."
      }

      //If user presses OK, FileHandler will call the API
      fighterListItem.onAction = (e: javafx.event.ActionEvent) =>
        val result = listRegenAlert.showAndWait()

        result match
          case Some(ButtonType.OK) =>
            try
              FileHandler.getFightersAPI()
            catch
              case _ => APIfailAlert.showAndWait()
          case _ =>

      menuBar.menus = List(file, fighters)

      val rootPane = new BorderPane
      rootPane.top = menuBar

      val uiPane = new ScrollPane {
        this.fitToHeight = true
        this.pannable = true
        //this.vbarPolicy = ScrollPane.ScrollBarPolicy.Never
        this.hbarPolicy = ScrollPane.ScrollBarPolicy.Always
      }

      val defaultElement = new DefaultElement
      val newElement = new Button() {
        val imgFile = new File("pics\\addicon.png")
        val img = new image.Image(imgFile.toURI.toString)
        val imgView = new ImageView()
        imgView.image = img
        imgView.fitHeight = 200
        imgView.fitWidth = 200
        this.setGraphic(imgView)
      }
      val elements = new HBox(defaultElement, newElement) {
        spacing = 10
        this.minWidth = (rootPane.width.value)
        padding = Insets(40, 10, 10, 10)
        this.fillHeight = false
        alignment = Pos.TopLeft
      }

      newElement.onAction = (e: javafx.event.ActionEvent) => {
        val childToAdd = new UIElementParent(elementControl)
        childToAdd.fighterData = fighterData
        childToAdd.getChild.update
        elementsBuffer += childToAdd
        elements.children.add(elements.children.size - 1, childToAdd)
        updateWidth
      }

      val selectedElements: Buffer[UIElementParent] = Buffer()

      var initialMouseCoords: (Double, Double) = (0, 0)

      var selectionRectangle = new scalafx.scene.shape.Rectangle {
        this.x = 0
        this.y = 0
        this.layoutX = 0
        this.layoutY = 0
        this.width = 100
        this.height = 100
        this.visible = false
        this.mouseTransparent = true
        stroke.value = Color.Blue
        fill.value = Color.color(0, 0, 1, 0.3)
      }

      val rootStack = new StackPane() {
      }

      uiPane.content = elements


      rootStack.children.add(uiPane)
      rootStack.children.add(selectionRectangle)
      rootStack.setAlignment(Pos.TopLeft)

      var canSelect = false

      var selecting = false

      val selectedElems: Buffer[UIElementParent] = Buffer()

      val intersecting: Buffer[UIElementParent] = Buffer()

      uiPane.onMousePressed = (e: javafx.scene.input.MouseEvent) =>
        if !selecting && canSelect then
          selectedElems.clear()
          intersecting.clear()
          initialMouseCoords = (e.getX, e.getY)
          selectionRectangle.visible = true
          selectionRectangle.setTranslateX(e.getX)
          selectionRectangle.setTranslateY(e.getY)
          selectionRectangle.setWidth(0)
          selectionRectangle.setHeight(0)
          selecting = true
          uiPane.pannable = false
        else
          uiPane.pannable = true
          selectedElems.clear()
          selectedElems.addAll(intersecting)
          selectionRectangle.visible = false
          selecting = false

      uiPane.onMouseMoved = (e: javafx.scene.input.MouseEvent) =>
        if selecting then
          val topleftCoords = (math.min(initialMouseCoords._1, e.getX), math.min(initialMouseCoords._2, e.getY))
          val bottomrightCoords = (math.max(initialMouseCoords._1, e.getX), math.max(initialMouseCoords._2, e.getY))
          selectionRectangle.setTranslateX(topleftCoords._1)
          selectionRectangle.setTranslateY(topleftCoords._2)
          selectionRectangle.setWidth(bottomrightCoords._1 - topleftCoords._1)
          selectionRectangle.setHeight(bottomrightCoords._2 - topleftCoords._2)
          intersecting.clear()
          intersecting.addAll(elementsBuffer.filter(elem => elem.intersects(elem.sceneToLocal(selectionRectangle.localToScene(selectionRectangle.getBoundsInLocal)))))

      uiPane.onKeyPressed = (e: javafx.scene.input.KeyEvent) =>
        e.getCode match
          case javafx.scene.input.KeyCode.CONTROL =>
            canSelect = true
          case javafx.scene.input.KeyCode.D =>
            if canSelect then
              selectedElems.foreach(elem => elem.deleteDuplicate(elem, false))
              selectedElems.clear()
          case javafx.scene.input.KeyCode.DELETE =>
            selectedElems.foreach(elem => elem.deleteDuplicate(elem, true))
            selectedElems.clear()
          case _ =>

      uiPane.onKeyReleased = (e: javafx.scene.input.KeyEvent) =>
        e.getCode match
          case javafx.scene.input.KeyCode.CONTROL =>
            canSelect = false
          case _ =>

      rootPane.center = rootStack

      rootPane.prefWidth = (800)
      rootPane.prefHeight = (500)

      scene = new Scene {
        root = rootPane
      }

      def updateWidth =
        uiPane.resize(1000 + elementsBuffer.map(elem => elem.getChild.size + elem.getChild.paddingX).sum, uiPane.height.value)

      def elementControl(elem: UIElementParent, deleteOrDuplicate: Boolean) =
        if deleteOrDuplicate then
          elementsBuffer -= elem
          elements.children.remove(elements.children.indexOf(elem))
          updateWidth
        else
          val elemToAdd = DataParser.duplicateElement(elem)
          elemToAdd.fighterData = fighterData
          elemToAdd.getChild.update
          elementsBuffer.insert(elementsBuffer.indexOf(elem), elemToAdd)
          elements.children.add(elements.children.indexOf(elem), elemToAdd)
          updateWidth
    }
}