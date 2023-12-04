describe('List Building Floors', () => {
  
    beforeEach(() => {
      //criar edificio para ver na tabela
      cy.visit('http://localhost:4200/login');
      cy.get('#role').select('Administrador');
      cy.get('#signUp').click();
      cy.get('[id^="Campus"]').click();
      cy.get('[routerlink="create-building"]').click();
      const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
      cy.get('#mat-input-0').type('A'+numeroAleatorio.toString());
      cy.get('#mat-input-1').type('10');
      cy.get('#mat-input-2').type('50');
      cy.get('#mat-input-3').type('Edificio A');
      cy.get('#mat-input-4').type('Edificio A - Administracao');
      cy.get('.mdc-button__label').click();
      //ir para a pagina correta do teste
      cy.get('[id^="Campus"]').click();
      cy.get('[routerlink="list-building-floors"]').click();
    });

    beforeEach(() => {
      cy.request('http://localhost:4000/api/floors'); 
    });

    it('should display a list of floors', () => {
        cy.request('http://localhost:4000/api/floors').then((response) => {
          expect(response.status).to.eq(200); // Ensure the request was successful
      
          const floors = response.body;
          expect(floors).to.have.length.greaterThan(0); // Ensure there are buildings
        });
      });
      
    it('should update table when a building is selected', () => {
      // Assuming a building selection triggers an API call to get floors
      cy.get('#mat-select-0').click();  // Click on the dropdown to open it
      cy.get('mat-option').first().click()
    });

  
    it('should display floors for a selected building', () => {
      cy.get('#mat-select-0').click();
      cy.get('.mat-mdc-select-placeholder').click({force: true });

      cy.contains('B1234 - 68e01dff-4c74-4673-a42d-9987947870d3').click();
            
      cy.get('table.mat-elevation-z8').should('exist');
      cy.get('.mat-mdc-header-row > .cdk-column-id').should('exist');
      cy.get('.mat-mdc-header-row > .cdk-column-floorNumber').should('exist');
      cy.get('.mat-mdc-header-row > .cdk-column-width').should('exist');
      cy.get('.mat-mdc-header-row > .cdk-column-length').should('exist');
      cy.get('.building-floor-row > .cdk-column-id').should('not.be.empty');
      cy.get('.building-floor-row  > .cdk-column-floorNumber').should('not.be.empty');
      cy.get('.building-floor-row > .cdk-column-width').should('not.be.empty');
      cy.get('.building-floor-row  > .cdk-column-length').should('not.be.empty');
      });
});
