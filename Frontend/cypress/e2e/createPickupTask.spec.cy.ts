describe('Teste de criar uma Tarefa de vigilancia', () => {
  
    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-9').type('A'+numeroAleatorio.toString());
        cy.get('#mat-input-10').type("10");
        cy.get('#mat-input-11').type('10');
        cy.get('#mat-input-12').type('teste');
        cy.get('#mat-input-13').type('Mais um edificio de teste');
        cy.get('#submitButtonBuilding').click()
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-floor"]').click();
        cy.get('#dropdownCreateFloor').click().get('mat-option').last().click();
        cy.get('#mat-input-14').type("10");
        cy.get('#mat-input-15').type('10');
        cy.get('#mat-input-16').type('4');
        cy.get('#mat-input-17').type('Mais um floor de teste');
        cy.get('#saveNewFloor').click();
        cy.get('[id^="Tasks"]').click();
        cy.get('[routerlink="create-task"]').click();
    });
  
    
    it('Deve ser criada uma tarefa de pickup com sucesso.', () => {
        cy.intercept('POST', 'http://localhost:3000/api/tasks/pickupDelivery').as('rotaFloor');

        cy.get('#select-task-dropdown').click();
        cy.get('#optionPickup').click();
        cy.get('#pickupTask-dropdownBuildingPickup').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownFloorPickup').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownBuildingPickup-2').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownFloorDelivery2').click().get('mat-option').last().click();
        cy.get('#pickupTasklocalizationx').type("10");
        cy.get('#pickupTasklocalizationy').type("10");
        cy.get('#pickupTaskName').type("10");
        cy.get('#pickupTaskContact').type("10");
        cy.get('#pickupTasklocalizationx2').type("10");
        cy.get('#pickupTasklocalizationy2').type("10");
        cy.get('#pickupTaskName').type("10");
        cy.get('#pickupTaskContact').type("10");
        cy.get('#pickupTaskName2').type("10");
        cy.get('#pickupTaskContact2').type("10");
        cy.get('#createTaskPickupDescription').type("10");
        cy.get('#pickupTasksubmitbutton').click()
        
        cy.wait('@rotaFloor').should(({ response }) => {
            expect(response!.statusCode).to.eq(200); 
        });
    });

    it('Deve falhar ao criar tarefa sem todos os campos.', () => {
        cy.intercept('POST', 'http://localhost:3000/api/tasks/pickupDelivery').as('rotaFloor');

        cy.get('#select-task-dropdown').click();
        cy.get('#optionPickup').click();
        cy.get('#pickupTask-dropdownBuildingPickup').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownFloorPickup').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownBuildingPickup-2').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownFloorDelivery2').click().get('mat-option').last().click();
        cy.get('#pickupTaskName').type("10");
        cy.get('#pickupTaskContact').type("10");
        cy.get('#pickupTasklocalizationx2').type("10");
        cy.get('#pickupTasksubmitbutton').click()
        
        cy.wait('@rotaFloor').should(({ response }) => {
            expect(response!.statusCode).to.eq(400); 
        });
    });
  
  });