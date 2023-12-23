import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatSnackBar } from '@angular/material/snack-bar';
import { FormBuilder, ReactiveFormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';
import { Observable, of } from 'rxjs';
import { CreateTaskComponent } from 'src/app/home/tasks/create-task/create-task.component';
import { BuildingService } from 'src/app/services/building.service';
import { FloorService } from 'src/app/services/floor.service';
import { TasksService } from 'src/app/services/tasks.service';

describe('CreateTaskComponent', () => {
  let component: CreateTaskComponent;
  let fixture: ComponentFixture<CreateTaskComponent>;

  // Mock services
  const mockBuildingService = {
    getAllBuildings: () => of([]),
  };

  const mockFloorService = {
    getFloorsAtBuildings: (building: string, someParam: boolean) => of([]),
  };

  const mockSnackBar = {
    open: jasmine.createSpy(),
  };

  const mockTaskService = {
    createVigilanceTask: (taskForm: any) => of({}),
    createPickupTask: (taskForm: any) => of({}),
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [CreateTaskComponent],
      imports: [ReactiveFormsModule, HttpClientModule],
      providers: [
        FormBuilder,
        { provide: BuildingService, useValue: mockBuildingService },
        { provide: FloorService, useValue: mockFloorService },
        { provide: MatSnackBar, useValue: mockSnackBar },
        { provide: TasksService, useValue: mockTaskService },
      ],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateTaskComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should handle building selection for vigilancia task', () => {
    const building = 'Building 1';
    spyOn(mockFloorService, 'getFloorsAtBuildings').and.returnValue(of([]));

    component.onBuildingSelector(building);

    expect(mockFloorService.getFloorsAtBuildings).toHaveBeenCalledWith(building, true);
    expect(component.floors.length).toBe(0);
  });

  it('should handle building selection for pickup task', () => {
    const building = 'Building 1';
    spyOn(mockFloorService, 'getFloorsAtBuildings').and.returnValue(of([]));

    component.onBuildingPickupSelector(building);

    expect(mockFloorService.getFloorsAtBuildings).toHaveBeenCalledWith(building, true);
    expect(component.pickupFloors.length).toBe(0);
  });

  it('should handle building selection for delivery task', () => {
    const building = 'Building 1';
    spyOn(mockFloorService, 'getFloorsAtBuildings').and.returnValue(of([]));

    component.onBuildingDeliverySelector(building);

    expect(mockFloorService.getFloorsAtBuildings).toHaveBeenCalledWith(building, true);
    expect(component.deliveryFloors.length).toBe(0);
  });

  it('should submit vigilancia task form', () => {
    component.selectedTask = 'vigilancia';
    spyOn(mockTaskService, 'createVigilanceTask').and.returnValue(of({}));

    component.onSubmit();

    expect(mockTaskService.createVigilanceTask).toHaveBeenCalled();
    expect(mockSnackBar.open).toHaveBeenCalledWith('Tarefa criada com sucesso!', 'close', {
      duration: 5000,
      panelClass: ['snackbar-success'],
    });
  });

  it('should submit pickup task form', () => {
    component.selectedTask = 'pickup';
    spyOn(mockTaskService, 'createPickupTask').and.returnValue(of({}));

    component.onSubmit();

    expect(mockTaskService.createPickupTask).toHaveBeenCalled();
  });
  
})
