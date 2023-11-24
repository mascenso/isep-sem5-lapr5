import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AddMapFloorComponent } from './add-map-floor.component';

describe('AddMapFloorComponent', () => {
  let component: AddMapFloorComponent;
  let fixture: ComponentFixture<AddMapFloorComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [AddMapFloorComponent]
    });
    fixture = TestBed.createComponent(AddMapFloorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
