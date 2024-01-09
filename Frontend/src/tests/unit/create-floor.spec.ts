import { HttpClientModule } from "@angular/common/http";
import { ComponentFixture, TestBed } from "@angular/core/testing";
import { FormBuilder, FormGroup, ReactiveFormsModule } from "@angular/forms";
import { MatSnackBar } from "@angular/material/snack-bar";
import { CreateFloorComponent } from "src/app/home/campus/create-floor/create-floor.component";
import { BuildingService } from "src/app/services/building.service";
import { FloorService } from "src/app/services/floor.service";
import { of } from 'rxjs';


describe('Create Floor Component', () => {
    let component: CreateFloorComponent;
    let fixture: ComponentFixture<CreateFloorComponent>;
  
    // Mock services
    const mockBuildingService = {
      getAllBuildings: () => of([]),
    };
  
    const mockFloorService = {
        createFloor: (floor: object) => of([{ name: 'yourStringValue' }])
      
    };
  
    const mockSnackBar = {
      open: jasmine.createSpy(),
    };

    beforeEach(async () => {
        await TestBed.configureTestingModule({
          declarations: [CreateFloorComponent],
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
        fixture = TestBed.createComponent(CreateFloorComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create component', () => {
        expect(component).toBeTruthy();
    });

    it('should call "createFloor" with arguments if have all data', () => {
        const dataForForm = {buildingId:"",width:1,length:2,floorNumber:1,description:"floor",floorMap:{}};
        component.floorForm.setValue(dataForForm)
        spyOn(mockFloorService, 'createFloor').and.returnValue(of([{name:"floor1"}]));

        component.onSubmit();

        expect(mockFloorService.createFloor).toHaveBeenCalled();
        expect(mockFloorService.createFloor).toHaveBeenCalledWith(dataForForm);
    });

})