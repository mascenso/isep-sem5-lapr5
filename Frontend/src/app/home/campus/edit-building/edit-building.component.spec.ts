import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EditBuildingComponent } from './edit-building.component';
import { FormBuilder } from '@angular/forms';
import { BuildingService } from 'src/app/services/building.service';
import { of } from 'rxjs';

describe('EditBuildingComponent', () => {
  let component: EditBuildingComponent;
  let fixture: ComponentFixture<EditBuildingComponent>;

  // Mock services
  const mockBuildingService = {
    getAllBuildings: () => of([]),
    editBuilding:(building: {}) => of([])
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [EditBuildingComponent],
      providers: [
        FormBuilder,
        { provide: BuildingService, useValue: mockBuildingService }
      ],
    });
    fixture = TestBed.createComponent(EditBuildingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create a component', () => {
    expect(component).toBeTruthy();
  });

  it('should call "editBuilding" with arguments when click to submit', () => {
    const buildingData = {id:"11",code:"1",maxWidth:1,maxLength:2,name:"building",description:"edificio"}
    spyOn(mockBuildingService, 'getAllBuildings').and.returnValue(of([]));
    spyOn(mockBuildingService, 'editBuilding').and.returnValue(of([]));
    component.buildingForm.setValue(buildingData)
    component.onSubmit()
    expect(mockBuildingService.editBuilding).toHaveBeenCalled();
    expect(mockBuildingService.editBuilding).toHaveBeenCalledWith(buildingData);
  });

  it('should call getAllBuildings on service', () => {
    spyOn(mockBuildingService, 'getAllBuildings').and.returnValue(of([]));
    component.getBuildings()
    expect(mockBuildingService.getAllBuildings).toHaveBeenCalled();
  });
});
