import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatTableModule } from '@angular/material/table';
import { CreateBuildingComponent } from '../../app/home/campus/create-building/create-building.component';
import { FormBuilder } from '@angular/forms';
import { BuildingService } from 'src/app/services/building.service';
import { of } from 'rxjs';

describe('CreateBuildingComponent', () => {
  let component: CreateBuildingComponent;
  let fixture: ComponentFixture<CreateBuildingComponent>;

  // Mock services
  const mockBuildingService = {
    getAllBuildings: () => of([]),
    createBuilding:(object:object,boolean:boolean) => of([])
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [CreateBuildingComponent],
      providers: [
        FormBuilder,
        { provide: BuildingService, useValue: mockBuildingService }
      ],
    });
    fixture = TestBed.createComponent(CreateBuildingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create a component', () => {
    expect(component).toBeTruthy();
  });


  it('should call createBuilding when submit all data', () => {
    spyOn(mockBuildingService, 'createBuilding').and.returnValue(of([]));

    component.buildingForm.setValue({
      code: 'B1234',
      name: 'Building Name',
      maxWidth: "10",
      maxLength: "20",
      description: 'Building Description',
    });

    component.onSubmit();

    expect(mockBuildingService.createBuilding).toHaveBeenCalledWith(
      jasmine.objectContaining({
        code: 'B1234',
        name: 'Building Name',
        maxWidth: "10",
        maxLength: "20",
        description: 'Building Description',
      }),
      true
    );

  });
});
