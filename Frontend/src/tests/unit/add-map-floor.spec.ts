import { ComponentFixture, TestBed, fakeAsync, tick } from '@angular/core/testing';
import { MatSnackBar } from '@angular/material/snack-bar';
import { AddMapFloorComponent } from 'src/app/home/campus/add-map-floor/add-map-floor.component';
import { BuildingService } from 'src/app/services/building.service';
import { FloorService } from 'src/app/services/floor.service';
import { of } from 'rxjs';
import { BuildingResponseDTO } from "src/dto/buildingDTO";
import { HttpClientModule } from '@angular/common/http';
import { FormBuilder, ReactiveFormsModule } from '@angular/forms';

describe('AddMapComponent', () => {
  let component: AddMapFloorComponent;
  let fixture: ComponentFixture<AddMapFloorComponent>;

  // Mock services
  const mockBuildingService = {
    getAllBuildings: () => of([]),
  };

  const mockFloorService = {
    getFloorsAtBuildings: (building: string) => of([]),
    addMapFloor:(floor: string, map: object)=> of([])
  };

  const mockSnackBar = {
    open: jasmine.createSpy(),
  };


  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [AddMapFloorComponent],
      imports: [ReactiveFormsModule, HttpClientModule],
      providers: [
        FormBuilder,
        { provide: BuildingService, useValue: mockBuildingService },
        { provide: FloorService, useValue: mockFloorService },
        { provide: MatSnackBar, useValue: mockSnackBar },
      ],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AddMapFloorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should put new file on fileToUpload variable', () => {
    const content = 'fake file content'; // Conteúdo fictício do arquivo, se necessário
    const blob = new Blob([content], { type:'text/plain' });
    const file = new File([blob], "fakefile.txt", { type:"text/plain" });
    //garantir que esta vazia a varivel
    component.fileToUpload = null;

    component.onFileSelected(file);

    expect(component.fileToUpload).not.toEqual(null);
  });

  it('should call service for get floors at building', () => {
    const building = "123456789"
    spyOn(mockFloorService, 'getFloorsAtBuildings').and.returnValue(of([]));
    component.getFloorsAtBuilding(building);

    expect(mockFloorService.getFloorsAtBuildings).toHaveBeenCalled();
    expect(mockFloorService.getFloorsAtBuildings).toHaveBeenCalledWith(building);
  });

  it('should call service for get all buildings', () => {
    const building = "123456789"
    spyOn(mockBuildingService, 'getAllBuildings').and.returnValue(of([]));
    component.getAllBuildings();

    expect(mockBuildingService.getAllBuildings).toHaveBeenCalled();

  });

  it('should call addMapFloor with map to submit', () => {
    const floor = "123456789"
    const content = 'fake file content'; // Conteúdo fictício do arquivo, se necessário
    const blob = new Blob([content], { type:'text/plain' });
    const file = new File([blob], "fakefile.txt", { type:"text/plain" });
    spyOn(mockFloorService, 'addMapFloor').and.returnValue(of([]));

    component.selectedBuilding = "building 1"
    component.selectedFloor = "floor1"
    component.fileToUpload = file

    component.submitMap();

    expect(mockFloorService.addMapFloor).toHaveBeenCalled();
    expect(mockFloorService.addMapFloor).toHaveBeenCalledWith("floor1", file);

  });

  it('should fail on call addMapFloor without floor', () => {
    const floor = "123456789"
    const content = 'fake file content'; // Conteúdo fictício do arquivo, se necessário
    const blob = new Blob([content], { type:'text/plain' });
    const file = new File([blob], "fakefile.txt", { type:"text/plain" });
    spyOn(mockFloorService, 'addMapFloor').and.returnValue(of([]));

    component.selectedBuilding = "building 1"

    component.fileToUpload = file

    component.submitMap();

    expect(mockFloorService.addMapFloor).not.toHaveBeenCalled();
  });
  it('should fail on call addMapFloor without building', () => {
    const floor = "123456789"
    const content = 'fake file content'; // Conteúdo fictício do arquivo, se necessário
    const blob = new Blob([content], { type:'text/plain' });
    const file = new File([blob], "fakefile.txt", { type:"text/plain" });
    spyOn(mockFloorService, 'addMapFloor').and.returnValue(of([]));


    component.selectedFloor = "floor1"
    component.fileToUpload = file

    component.submitMap();

    expect(mockFloorService.addMapFloor).not.toHaveBeenCalled();
  });
  it('should fail on call addMapFloor without file', () => {
    const floor = "123456789"
    const content = 'fake file content'; // Conteúdo fictício do arquivo, se necessário
    const blob = new Blob([content], { type:'text/plain' });
    const file = new File([blob], "fakefile.txt", { type:"text/plain" });
    spyOn(mockFloorService, 'addMapFloor').and.returnValue(of([]));

    component.selectedBuilding = "building 1"
    component.selectedFloor = "floor1"


    component.submitMap();

    expect(mockFloorService.addMapFloor).not.toHaveBeenCalled();
  });
  
})

