import { MatSnackBar } from '@angular/material/snack-bar';
import { BuildingService } from 'src/app/services/building.service';
import { ElevatorService } from 'src/app/services/elevator.service';
import { Observable, of, throwError } from 'rxjs';
import { BuildingResponseDTO } from 'src/dto/buildingDTO';
import { ElevatorResponseDTO, CreateElevatorDTO } from 'src/dto/elevatorDTO';
import { ListElevatorsComponent } from 'src/app/home/campus/list-elevators/list-elevators.component';
import { TestBed } from '@angular/core/testing';

describe('ListElevatorsComponent', () => {
  let component: ListElevatorsComponent;
  let buildingService: jasmine.SpyObj<BuildingService>;
  let elevatorService: jasmine.SpyObj<ElevatorService>;
  let snackBar: jasmine.SpyObj<MatSnackBar>;

  beforeEach(() => {
    buildingService = jasmine.createSpyObj('BuildingService', ['getAllBuildings']);
    elevatorService = jasmine.createSpyObj('ElevatorService', ['getBuildingElevators', 'createElevator']);
    snackBar = jasmine.createSpyObj('MatSnackBar', ['open']);
    //elevatorService = TestBed.inject(ElevatorService) as jasmine.SpyObj<ElevatorService>;

    component = new ListElevatorsComponent(buildingService, elevatorService, snackBar);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  
  it('should handle errors when fetching elevators for building', () => {
    const selection = 'BuildingId';

    elevatorService.getBuildingElevators.and.returnValue(throwError('Error'));

    component.onSelectionUpdateTable(selection);

    expect(snackBar.open).toHaveBeenCalledWith("That building doesn't have an elevator!", 'close', {
      duration: 5000,
      panelClass: ['snackbar-error']
    });
  });


  it('should get elevators for building on selection', (done) => {
    const selection = 'BuildingId'; 
  
    const elevatorsMock: ElevatorResponseDTO = {
      id: 'ElevatorId1',
      code: 'ElevatorCode1',
      floorList: ['Floor1', 'Floor2'],
      buildingId: 'BuildingId'
    };
  
    const mockObservable: Observable<ElevatorResponseDTO> = of(elevatorsMock);
    elevatorService.getBuildingElevators.and.returnValue(mockObservable);
  
    component.onSelectionUpdateTable(selection);
  
    expect(elevatorService.getBuildingElevators).toHaveBeenCalledWith(selection, true);
  
    // Aguarda a atualização da propriedade dataSource
    setTimeout(() => {
      expect(component.dataSource).toEqual([elevatorsMock]); // Verifica se a dataSource foi atualizada corretamente
      done();
    }, 0);
  });

  afterEach(() => {
    component.ngOnDestroy();
  });
});
